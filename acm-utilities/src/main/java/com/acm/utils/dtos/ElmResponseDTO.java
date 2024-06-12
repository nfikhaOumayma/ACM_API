package com.acm.utils.dtos;

/**
 * This class represents a Data Transfer Object (DTO) for a ELM with transaction ID and random
 * value.
 */
public class ElmResponseDTO {
	private String transId;
	private String random;

	/**
	 * Gets the transaction ID.
	 *
	 * @return The transaction ID.
	 */
	public String getTransId() {

		return transId;
	}

	/**
	 * Sets the transaction ID.
	 *
	 * @param transId The transaction ID to set.
	 */
	public void setTransId(String transId) {

		this.transId = transId;
	}

	/**
	 * Gets the random value.
	 *
	 * @return The random value.
	 */
	public String getRandom() {

		return random;
	}

	/**
	 * Sets the random value.
	 *
	 * @param random The random value to set.
	 */
	public void setRandom(String random) {

		this.random = random;
	}

	/**
	 * Returns a string representation of the TransactionDTO object.
	 *
	 * @return A string representation of the TransactionDTO.
	 */
	@Override
	public String toString() {

		return "TransactionDTO{" + "transId='" + transId + '\'' + ", random='" + random + '\''
				+ '}';
	}
}
