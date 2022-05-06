import {
  Lucid,
  Tx,
  Blockfrost,
  Data,
  Construct,
  validatorToAddress,
  validatorToScriptHash,
  Redeemer,
  Datum,
} from "lucid-cardano";
import { project_id } from "../../../secrets"; // create a file secrets.ts at the root of the and export the project_id
import scripts from "./scripts.json";

await Lucid.initialize(
  new Blockfrost("https://cardano-mainnet.blockfrost.io/api/v0", project_id)
);

const { tokenValidator, tokenPolicy, threadPolicy } = scripts;

const contractAddress = validatorToAddress({
  type: "PlutusV1",
  script: tokenValidator,
});

const tokenPolicyId = validatorToScriptHash({
  type: "PlutusV1",
  script: tokenPolicy,
});
const threadPolicyId = validatorToScriptHash({
  type: "PlutusV1",
  script: threadPolicy,
});

const ownershipPolicy =
  "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc";

const assetPrefix = "budz";

/** This input is necessary in the deploy tx to mint the unique thread token */
const oref = {
  txHash: "08fc5aa81e5c4b44291ae291f7c8c33eac7345ff5bfe315d26419ae63f62a82b",
  outputIndex: 1,
};

const Count = (c: number): Datum => Data.from(BigInt(c));

const Action = {
  Mint: Data.from(new Construct(0, [])) as Redeemer,
  Burn: Data.from(new Construct(1, [])) as Redeemer,
};

export const deploy = async () => {
  const utxos = await Lucid.wallet.getUtxos();
  const utxo = utxos.find(
    (utxo) =>
      utxo.txHash === oref.txHash && utxo.outputIndex === oref.outputIndex
  );
  if (!utxo) throw new Error("Utxo is required to deploy NFT contract");

  const tx = await Tx.new()
    .collectFrom([utxo])
    .mintAssets({ [threadPolicyId]: 1n }, Data.empty())
    .payToContract(contractAddress, Count(0), { [threadPolicyId]: 1n })
    .attachMintingPolicy({ type: "PlutusV1", script: threadPolicy })
    .complete();

  const signedTx = (await tx.sign()).complete();

  const txHash = await signedTx.submit();
  return txHash;
};

export const mint = async (metadata: any) => {
  const utxo = (
    await Lucid.utxosAtWithUnit(contractAddress, threadPolicyId)
  )[0];

  /** This request for datum discovery will soon be integrated into Lucid itself */
  const currentCount = await fetch(
    `https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/${utxo.datumHash}`,
    { headers: { project_id } }
  )
    .then((res) => res.json())
    .then((res) => parseInt(res.json_value.int));

  utxo.datum = Count(currentCount);

  const walletUtxos = await Lucid.wallet.getUtxos();
  const ownershipUtxo = walletUtxos.find((utxo) =>
    Object.keys(utxo.assets).some((unit) => unit.startsWith(ownershipPolicy))
  );

  if (!ownershipUtxo) throw new Error("You are not eligible to mint an NFT");

  const mintToken =
    tokenPolicyId + Buffer.from(assetPrefix + currentCount).toString("hex");

  const tx = await Tx.new()
    .collectFrom([ownershipUtxo])
    .collectFrom([utxo], Data.empty())
    .mintAssets({ [mintToken]: 1n }, Action.Mint)
    .payToContract(contractAddress, Count(currentCount + 1), {
      [threadPolicyId]: 1n,
    })
    .attachMetadata(721, {
      [tokenPolicyId]: {
        [assetPrefix + currentCount]: {
          ...metadata,
        },
      },
    })
    .attachMintingPolicy({ type: "PlutusV1", script: tokenPolicy })
    .attachMintingPolicy({ type: "PlutusV1", script: tokenValidator })
    .complete();

  const signedTx = (await tx.sign()).complete();

  const txHash = await signedTx.submit();
  return txHash;
};

export const burn = async (id: number) => {
  const unit = tokenPolicyId + Buffer.from(assetPrefix + id).toString("hex");
  const tx = await Tx.new()
    .mintAssets({ [unit]: -1n }, Action.Burn)
    .attachMintingPolicy({ type: "PlutusV1", script: tokenPolicy })
    .complete();
  const signedTx = (await tx.sign()).complete();

  const txHash = await signedTx.submit();
  return txHash;
};
