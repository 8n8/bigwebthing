int main_err();

int main() {
	int err = main_err();
	if err != nil {
		printf(show_error(err));
	}
}

int main_err() {
	hydro_kx_keypair static_keys;
	int err = get_static_keys(&static_keys);
	if (err) {
		return err;
	}
}
