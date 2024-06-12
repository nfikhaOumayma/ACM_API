/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * {@link OauthAccessToken} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Entity
@Table(name = "oauth_access_token")
@NamedQuery(name = "OauthAccessToken.findAll", query = "SELECT o FROM OauthAccessToken o")
public class OauthAccessToken implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2910464609060848122L;

	/** The authentication id. */
	@Id
	@Column(name = "authentication_id", unique = true, nullable = false, length = 256)
	private String authenticationId;

	/** The authentication. */
	private byte[] authentication;

	/** The client id. */
	@Column(name = "client_id", length = 256)
	private String clientId;

	/** The refresh token. */
	@Column(name = "refresh_token", length = 256)
	private String refreshToken;

	/** The token. */
	private byte[] token;

	/** The token id. */
	@Column(name = "token_id", length = 256)
	private String tokenId;

	/** The user name. */
	@Column(name = "user_name", length = 256)
	private String userName;

	/**
	 * Instantiates a new oauth access token.
	 */
	public OauthAccessToken() {

		/*
		 * Empty
		 */
	}

	/**
	 * Gets the authentication id.
	 *
	 * @return the authentication id
	 */
	public String getAuthenticationId() {

		return this.authenticationId;
	}

	/**
	 * Sets the authentication id.
	 *
	 * @param authenticationId the new authentication id
	 */
	public void setAuthenticationId(String authenticationId) {

		this.authenticationId = authenticationId;
	}

	/**
	 * Gets the authentication.
	 *
	 * @return the authentication
	 */
	public byte[] getAuthentication() {

		return this.authentication;
	}

	/**
	 * Sets the authentication.
	 *
	 * @param authentication the new authentication
	 */
	public void setAuthentication(byte[] authentication) {

		this.authentication = authentication;
	}

	/**
	 * Gets the client id.
	 *
	 * @return the client id
	 */
	public String getClientId() {

		return this.clientId;
	}

	/**
	 * Sets the client id.
	 *
	 * @param clientId the new client id
	 */
	public void setClientId(String clientId) {

		this.clientId = clientId;
	}

	/**
	 * Gets the refresh token.
	 *
	 * @return the refresh token
	 */
	public String getRefreshToken() {

		return this.refreshToken;
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
	 * Gets the token.
	 *
	 * @return the token
	 */
	public byte[] getToken() {

		return this.token;
	}

	/**
	 * Sets the token.
	 *
	 * @param token the new token
	 */
	public void setToken(byte[] token) {

		this.token = token;
	}

	/**
	 * Gets the token id.
	 *
	 * @return the token id
	 */
	public String getTokenId() {

		return this.tokenId;
	}

	/**
	 * Sets the token id.
	 *
	 * @param tokenId the new token id
	 */
	public void setTokenId(String tokenId) {

		this.tokenId = tokenId;
	}

	/**
	 * Gets the user name.
	 *
	 * @return the user name
	 */
	public String getUserName() {

		return this.userName;
	}

	/**
	 * Sets the user name.
	 *
	 * @param userName the new user name
	 */
	public void setUserName(String userName) {

		this.userName = userName;
	}

}
