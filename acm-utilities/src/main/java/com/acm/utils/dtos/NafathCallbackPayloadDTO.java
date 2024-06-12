package com.acm.utils.dtos;

/**
 * {@link NafathCallbackPayloadDTO} is a Data Transfer Object (DTO) class representing the payload
 * for Nafath callback requests. This class stores information such as a token, transaction ID, and
 * request ID.
 *
 * @author nrmila
 * @since 0.1.0
 */
public class NafathCallbackPayloadDTO {
	private String token;
	private String transId;
	private String requestId;

	/**
	 * Get the token associated with the payload.
	 *
	 * @return The token as a string.
	 */
	public String getToken() {

		return token;
	}

	/**
	 * Set the token for the payload.
	 *
	 * @param token The token to set as a string.
	 */
	public void setToken(String token) {

		this.token = token;
	}

	/**
	 * Get the transaction ID associated with the payload.
	 *
	 * @return The transaction ID as a string.
	 */
	public String getTransId() {

		return transId;
	}

	/**
	 * Set the transaction ID for the payload.
	 *
	 * @param transId The transaction ID to set as a string.
	 */
	public void setTransId(String transId) {

		this.transId = transId;
	}

	/**
	 * Get the request ID associated with the payload.
	 *
	 * @return The request ID as a string.
	 */
	public String getRequestId() {

		return requestId;
	}

	/**
	 * Set the request ID for the payload.
	 *
	 * @param requestId The request ID to set as a string.
	 */
	public void setRequestId(String requestId) {

		this.requestId = requestId;
	}
}
