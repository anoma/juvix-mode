./gifs/%.gif: ./videos/%.mkv
	ffmpeg -i $< -filter_complex "[0:v] split [a][b];[a] palettegen [p];[b][p] paletteuse" $@

# convert all .mkv files to .gif
all: $(patsubst ./videos/%.mkv, ./gifs/%.gif, $(wildcard ./videos/*.mkv))

clean:
	rm -f ./gifs/*.gif
