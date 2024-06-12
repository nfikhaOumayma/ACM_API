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
 * {@link OauthRefreshToken} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Entity
@Table(name = "oauth_refresh_token")
@NamedQuery(name = "OauthRefreshToken.findAll", query = "SELECT o FROM OauthRefreshToken o")
public class OauthRefreshToken implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3802003607624283232L;

	/** The token id. */
	@Id
	@Column(name = "token_id", length = 256)
	private String tokenId;

	/** The authentication. */
	private byte[] authentication;

	/** The token. */
	private byte[] token;

	/**
	 * Instantiates a new oauth refresh token.
	 */
	public OauthRefreshToken() {

		/*
		 * Empty
		 */
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

}
