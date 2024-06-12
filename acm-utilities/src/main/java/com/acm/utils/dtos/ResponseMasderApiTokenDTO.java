/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ResponseMasderApiTokenDTO.
 */
public class ResponseMasderApiTokenDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1564520648687969174L;

	/** The access token. */
	@JsonProperty("access_token")
	private String accessToken;

	/** The token type. */
	@JsonProperty("token_type")
	private String tokenType;

	/** The expires in. */
	@JsonProperty("expires_in")
	private String expiresIn;

	/** The refresh token. */
	@JsonProperty("refresh_token")
	private String refreshToken;

	/** The refresh token expires in. */
	@JsonProperty("refresh_token_expires_in")
	private String refreshTokenExpiresIn;

	/** The refresh token status. */
	@JsonProperty("refresh_token_status")
	private String refreshTokenStatus;

	/** The refresh count. */
	@JsonProperty("refresh_count")
	private String refreshCount;

	/**
	 * Instantiates a new response masder api token DTO.
	 */
	public ResponseMasderApiTokenDTO() {

		super();
	}

	/**
	 * Gets the access token.
	 *
	 * @return the access token
	 */
	public String getAccessToken() {

		return accessToken;
	}

	/**
	 * Sets the access token.
	 *
	 * @param accessToken the new access token
	 */
	public void setAccessToken(String accessToken) {

		this.accessToken = accessToken;
	}

	/**
	 * Gets the token type.
	 *
	 * @return the token type
	 */
	public String getTokenType() {

		return tokenType;
	}

	/**
	 * Sets the token type.
	 *
	 * @param tokenType the new token type
	 */
	public void setTokenType(String tokenType) {

		this.tokenType = tokenType;
	}

	/**
	 * Gets the expires in.
	 *
	 * @return the expires in
	 */
	public String getExpiresIn() {

		return expiresIn;
	}

	/**
	 * Sets the expires in.
	 *
	 * @param expiresIn the new expires in
	 */
	public void setExpiresIn(String expiresIn) {

		this.expiresIn = expiresIn;
	}

	/**
	 * Gets the refresh token.
	 *
	 * @return the refresh token
	 */
	public String getRefreshToken() {

		return refreshToken;
	}

	/**
	 * Sets the refresh token.
	 *
	 * @param refreshToken the new refresh token
	 */
	public void setRefreshToken(String refreshToken) {

		this.refreshToken = refreshToken;
	}

	/**
	 * Gets the refresh token expires in.
	 *
	 * @return the refresh token expires in
	 */
	public String getRefreshTokenExpiresIn() {

		return refreshTokenExpiresIn;
	}

	/**
	 * Sets the refresh token expires in.
	 *
	 * @param refreshTokenExpiresIn the new refresh token expires in
	 */
	public void setRefreshTokenExpiresIn(String refreshTokenExpiresIn) {

		this.refreshTokenExpiresIn = refreshTokenExpiresIn;
	}

	/**
	 * Gets the refresh token status.
	 *
	 * @return the refresh token status
	 */
	public String getRefreshTokenStatus() {

		return refreshTokenStatus;
	}

	/**
	 * Sets the refresh token status.
	 *
	 * @param refreshTokenStatus the new refresh token status
	 */
	public void setRefreshTokenStatus(String refreshTokenStatus) {

		this.refreshTokenStatus = refreshTokenStatus;
	}

	/**
	 * Gets the refresh count.
	 *
	 * @return the refresh count
	 */
	public String getRefreshCount() {

		return refreshCount;
	}

	/**
	 * Sets the refresh count.
	 *
	 * @param refreshCount the new refresh count
	 */
	public void setRefreshCount(String refreshCount) {

		this.refreshCount = refreshCount;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ResponseMasderApiTokenDTO [accessToken=" + accessToken + ", tokenType=" + tokenType
				+ ", expiresIn=" + expiresIn + ", refreshToken=" + refreshToken
				+ ", refreshTokenExpiresIn=" + refreshTokenExpiresIn + ", refreshTokenStatus="
				+ refreshTokenStatus + ", refreshCount=" + refreshCount + "]";
	}

}
