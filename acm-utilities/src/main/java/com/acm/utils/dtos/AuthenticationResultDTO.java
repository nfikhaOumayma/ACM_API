/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class AuthenticationResultDTO.
 */
public class AuthenticationResultDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4488731972918368664L;

	/** The access token. */
	@JsonProperty("AccessToken")
	public String accessToken;

	/** The expires in. */
	@JsonProperty("ExpiresIn")
	public int expiresIn;

	/** The id token. */
	@JsonProperty("IdToken")
	public String idToken;

	/** The refresh token. */
	@JsonProperty("RefreshToken")
	public String refreshToken;

	/** The token type. */
	@JsonProperty("TokenType")
	public String tokenType;

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
	 * Gets the expires in.
	 *
	 * @return the expires in
	 */
	public int getExpiresIn() {

		return expiresIn;
	}

	/**
	 * Sets the expires in.
	 *
	 * @param expiresIn the new expires in
	 */
	public void setExpiresIn(int expiresIn) {

		this.expiresIn = expiresIn;
	}

	/**
	 * Gets the id token.
	 *
	 * @return the id token
	 */
	public String getIdToken() {

		return idToken;
	}

	/**
	 * Sets the id token.
	 *
	 * @param idToken the new id token
	 */
	public void setIdToken(String idToken) {

		this.idToken = idToken;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AuthenticationResultDTO [accessToken=" + accessToken + ", expiresIn=" + expiresIn
				+ ", idToken=" + idToken + ", refreshToken=" + refreshToken + ", tokenType="
				+ tokenType + "]";
	}

}
