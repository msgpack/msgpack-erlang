defmodule Msgpack.Mixfile do
  use Mix.Project

  def project do
    [app: :msgpack,
     version: "0.3.5",
     language: :erlang,
     description: description,
     package: package,
     deps: deps]
  end

  defp deps do
    []
  end

  defp description do
    "MessagePack (de)serializer implementation for Erlang"
  end

  defp package do
    [files: ~w(AUTHORS include LICENSE-2.0.txt README.md rebar.config
               rebar.config.script src test),
     maintainers: ["UENISHI Kota"],
     licenses: ["Apache"],
     links: %{"Website" => "http://msgpack.org/",
              "GitHub" => "https://github.com/msgpack/msgpack-erlang"}]
   end
end
