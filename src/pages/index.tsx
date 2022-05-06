import { Lucid } from "lucid-cardano";
import * as React from "react";
const { burn, mint } =
  typeof window !== "undefined" ? await import("../cardano/nft") : ({} as any);

const IndexPage = () => {
  const [connected, setConnected] = React.useState(false);

  const [mintParams, setMintParams] = React.useState({
    image: "",
    name: "",
    description: "",
    loading: false,
  });

  const [burnParams, setBurnParams] = React.useState({
    id: "",
    loading: false,
  });

  const [result, setResult] = React.useState({ success: "", error: "" });
  const [waitingConfirmation, setWaitingConfirmation] = React.useState(false);

  const init = async () => {
    const connected = await (window as any)?.cardano?.nami?.isEnabled();
    if (connected) {
      await Lucid.selectWallet("nami");
    }
    setConnected(connected);
  };
  React.useEffect(() => {
    init();
  }, []);
  return (
    <div className="relative flex justify-center items-center w-full h-screen bg-slate-900 text-white flex-col">
      <div className="flex flex-col md:flex-row">
        <div className="w-[500px] max-w-[90%] h-[600px] shadow-sm bg-slate-800 rounded-xl flex items-center flex-col p-8">
          <div className="rounded-xl text-xl font-bold mb-6">
            Mint Plutus NFT
          </div>
          <div className="text-lg font-semibold">Image</div>
          <div className="text-xs w-[300px] mb-2">
            Insert an IPFS hash here. You need to upload the image yourself to
            the IPFS network.
          </div>
          <input
            onChange={(e) =>
              setMintParams((m) => ({ ...m, image: e.target.value }))
            }
            className="text-slate-900 outline-none p-2 rounded-xl w-3/4"
            placeholder="IPFS hash (Qm..)"
          />
          <div className="mt-6 mb-2 text-lg font-semibold">Name</div>
          <input
            onChange={(e) =>
              setMintParams((m) => ({ ...m, name: e.target.value }))
            }
            className="text-slate-900 outline-none p-2 rounded-xl w-3/4"
            placeholder="Name"
          />
          <div className="mt-6 mb-2 text-lg font-semibold">Description</div>
          <input
            onChange={(e) =>
              setMintParams((m) => ({ ...m, description: e.target.value }))
            }
            className="text-slate-900 outline-none p-2 rounded-xl w-3/4"
            placeholder="Description"
          />
          <div className="mt-10 text-sm mb-6 text-center">
            Only SpaceBudz holders are eligible to mint <br />
            (Max supply 10000)
          </div>
          <button
            onClick={async () => {
              const m = mintParams.description
                ? {
                    name: mintParams.name,
                    image: "ipfs://" + mintParams.image,
                    description: stringToArray(mintParams.description),
                  }
                : {
                    name: mintParams.name,
                    image: "ipfs://" + mintParams.image,
                  };
              setMintParams((m) => ({ ...m, loading: true }));
              const txHash = await mint(m).catch((e: any) => {
                setResult({
                  success: "",
                  error: e?.message || JSON.stringify(e),
                });
              });

              if (txHash) {
                setWaitingConfirmation(true);
                await Lucid.awaitTx(txHash);
                setResult({
                  success: `https://cardanoscan.io/transaction/${txHash}`,
                  error: "",
                });
                setWaitingConfirmation(false);
              }
              setMintParams((m) => ({ ...m, loading: false }));
            }}
            disabled={
              !connected ||
              !mintParams.image ||
              !mintParams.name ||
              mintParams.name.length > 64
            }
            className="rounded-xl bg-violet-800 py-4 px-8 hover:opacity-80 duration-200 disabled:opacity-30"
          >
            {mintParams.loading ? "..." : "Mint"}
          </button>
        </div>
        <div className="mt-8 md:mt-0 md:ml-8 w-[500px] max-w-[90%] h-[600px] shadow-sm bg-slate-800 rounded-xl flex items-center flex-col p-8">
          <div className="rounded-xl text-xl font-bold mb-6">
            Burn Plutus NFT
          </div>
          <div className="text-lg font-semibold">ID</div>
          <div className="text-xs w-[300px] mb-2">
            E.g. you own budz5 and want to burn it, then the ID is 5 you have to
            enter.
          </div>
          <input
            onChange={(e) =>
              setBurnParams((m) => ({ ...m, id: e.target.value }))
            }
            className="text-slate-900 outline-none p-2 rounded-xl w-3/4"
            placeholder="ID"
          />
          <button
            onClick={async () => {
              setBurnParams((b) => ({ ...b, loading: true }));
              const txHash = await burn(parseInt(burnParams.id)).catch(
                (e: any) => {
                  setResult({
                    success: "",
                    error: e?.message || JSON.stringify(e),
                  });
                }
              );
              if (txHash) {
                setWaitingConfirmation(true);
                await Lucid.awaitTx(txHash);
                setResult({
                  success: `https://cardanoscan.io/transaction/${txHash}`,
                  error: "",
                });
                setWaitingConfirmation(false);
              }
              setBurnParams((b) => ({ ...b, loading: false }));
            }}
            disabled={!connected || !burnParams.id}
            className="mt-10 rounded-xl bg-rose-800 py-4 px-8 hover:opacity-80 duration-200 disabled:opacity-30"
          >
            {burnParams.loading ? "..." : "Burn"}
          </button>
        </div>
        <button
          onClick={async () => {
            const connected = await (window as any)?.cardano?.nami?.enable();
            if (connected) {
              await Lucid.selectWallet("nami");
            }
            setConnected(connected);
          }}
          className="absolute right-7 top-7 border-2 rounded-xl border-white p-2  hover:bg-white hover:text-slate-900 duration-300"
        >
          {connected ? "Connected" : "Connect Nami"}
        </button>
      </div>
      {result.error && (
        <div className="mt-6 text-rose-700 text-sm">{result.error}</div>
      )}
      {waitingConfirmation && (
        <div className="mt-6 text-white text-sm">
          Waiting for transaction confirmation...
        </div>
      )}
      {result.success && (
        <a
          href={result.success}
          target="_blank"
          className="mt-6 text-white text-sm underline"
        >
          {result.success}
        </a>
      )}
    </div>
  );
};

const stringToArray = (str: string) => {
  const array = str.match(/.{1,64}/g)!;
  if (array?.length === 1) return array[0] as string;
  return array as string[];
};

export default IndexPage;
