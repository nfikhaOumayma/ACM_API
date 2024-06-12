package com.acm.utils.dtos;

/**
 * This class represents a Data Transfer Object (DTO) for a key.
 */
public class KeyDTO {
	private String kty;
	private String e;
	private String use;
	private String kid;
	private String alg;
	private String n;

	/**
	 * Gets the key type (kty).
	 *
	 * @return The key type.
	 */
	public String getKty() {

		return kty;
	}

	/**
	 * Sets the key type (kty).
	 *
	 * @param kty The key type to set.
	 */
	public void setKty(String kty) {

		this.kty = kty;
	}

	/**
	 * Gets the exponent (e).
	 *
	 * @return The exponent.
	 */
	public String getE() {

		return e;
	}

	/**
	 * Sets the exponent (e).
	 *
	 * @param e The exponent to set.
	 */
	public void setE(String e) {

		this.e = e;
	}

	/**
	 * Gets the key usage (use).
	 *
	 * @return The key usage.
	 */
	public String getUse() {

		return use;
	}

	/**
	 * Sets the key usage (use).
	 *
	 * @param use The key usage to set.
	 */
	public void setUse(String use) {

		this.use = use;
	}

	/**
	 * Gets the key ID (kid).
	 *
	 * @return The key ID.
	 */
	public String getKid() {

		return kid;
	}

	/**
	 * Sets the key ID (kid).
	 *
	 * @param kid The key ID to set.
	 */
	public void setKid(String kid) {

		this.kid = kid;
	}

	/**
	 * Gets the algorithm (alg).
	 *
	 * @return The algorithm.
	 */
	public String getAlg() {

		return alg;
	}

	/**
	 * Sets the algorithm (alg).
	 *
	 * @param alg The algorithm to set.
	 */
	public void setAlg(String alg) {

		this.alg = alg;
	}

	/**
	 * Gets the modulus (n).
	 *
	 * @return The modulus.
	 */
	public String getN() {

		return n;
	}

	/**
	 * Sets the modulus (n).
	 *
	 * @param n The modulus to set.
	 */
	public void setN(String n) {

		this.n = n;
	}

	/**
	 * Returns a string representation of the KeyDTO object.
	 *
	 * @return A string representation of the KeyDTO.
	 */
	@Override
	public String toString() {

		return "KeyDTO{" + "kty='" + kty + '\'' + ", e='" + e + '\'' + ", use='" + use + '\''
				+ ", kid='" + kid + '\'' + ", alg='" + alg + '\'' + ", n='" + n + '\'' + '}';
	}
}
