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
 * {@link OauthClientDetail} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Entity
@Table(name = "oauth_client_details")
@NamedQuery(name = "OauthClientDetail.findAll", query = "SELECT o FROM OauthClientDetail o")
public class OauthClientDetail implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 518318433830904842L;

	/** The client id. */
	@Id
	@Column(name = "client_id", unique = true, nullable = false, length = 256)
	private String clientId;

	/** The access token validity. */
	@Column(name = "access_token_validity")
	private Integer accessTokenValidity;

	/** The additional information. */
	@Column(name = "additional_information", length = 4096)
	private String additionalInformation;

	/** The authorities. */
	@Column(length = 256)
	private String authorities;

	/** The authorized grant types. */
	@Column(name = "authorized_grant_types", length = 256)
	private String authorizedGrantTypes;

	/** The autoapprove. */
	@Column(length = 256)
	private String autoapprove;

	/** The client secret. */
	@Column(name = "client_secret", length = 256)
	private String clientSecret;

	/** The refresh token validity. */
	@Column(name = "refresh_token_validity")
	private Integer refreshTokenValidity;

	/** The resource ids. */
	@Column(name = "resource_ids", length = 256)
	private String resourceIds;

	/** The scope. */
	@Column(length = 256)
	private String scope;

	/** The web server redirect uri. */
	@Column(name = "web_server_redirect_uri", length = 256)
	private String webServerRedirectUri;

	/**
	 * Instantiates a new oauth client detail.
	 */
	public OauthClientDetail() {

		/*
		 * Empty
		 */
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
	 * Gets the access token validity.
	 *
	 * @return the access token validity
	 */
	public Integer getAccessTokenValidity() {

		return this.accessTokenValidity;
	}

	/**
	 * Sets the access token validity.
	 *
	 * @param accessTokenValidity the new access token validity
	 */
	public void setAccessTokenValidity(Integer accessTokenValidity) {

		this.accessTokenValidity = accessTokenValidity;
	}

	/**
	 * Gets the additional information.
	 *
	 * @return the additional information
	 */
	public String getAdditionalInformation() {

		return this.additionalInformation;
	}

	/**
	 * Sets the additional information.
	 *
	 * @param additionalInformation the new additional information
	 */
	public void setAdditionalInformation(String additionalInformation) {

		this.additionalInformation = additionalInformation;
	}

	/**
	 * Gets the authorities.
	 *
	 * @return the authorities
	 */
	public String getAuthorities() {

		return this.authorities;
	}

	/**
	 * Sets the authorities.
	 *
	 * @param authorities the new authorities
	 */
	public void setAuthorities(String authorities) {

		this.authorities = authorities;
	}

	/**
	 * Gets the authorized grant types.
	 *
	 * @return the authorized grant types
	 */
	public String getAuthorizedGrantTypes() {

		return this.authorizedGrantTypes;
	}

	/**
	 * Sets the authorized grant types.
	 *
	 * @param authorizedGrantTypes the new authorized grant types
	 */
	public void setAuthorizedGrantTypes(String authorizedGrantTypes) {

		this.authorizedGrantTypes = authorizedGrantTypes;
	}

	/**
	 * Gets the autoapprove.
	 *
	 * @return the autoapprove
	 */
	public String getAutoapprove() {

		return this.autoapprove;
	}

	/**
	 * Sets the autoapprove.
	 *
	 * @param autoapprove the new autoapprove
	 */
	public void setAutoapprove(String autoapprove) {

		this.autoapprove = autoapprove;
	}

	/**
	 * Gets the client secret.
	 *
	 * @return the client secret
	 */
	public String getClientSecret() {

		return this.clientSecret;
	}

	/**
	 * Sets the client secret.
	 *
	 * @param clientSecret the new client secret
	 */
	public void setClientSecret(String clientSecret) {

		this.clientSecret = clientSecret;
	}

	/**
	 * Gets the refresh token validity.
	 *
	 * @return the refresh token validity
	 */
	public Integer getRefreshTokenValidity() {

		return this.refreshTokenValidity;
	}

	/**
	 * Sets the refresh token validity.
	 *
	 * @param refreshTokenValidity the new refresh token validity
	 */
	public void setRefreshTokenValidity(Integer refreshTokenValidity) {

		this.refreshTokenValidity = refreshTokenValidity;
	}

	/**
	 * Gets the resource ids.
	 *
	 * @return the resource ids
	 */
	public String getResourceIds() {

		return this.resourceIds;
	}

	/**
	 * Sets the resource ids.
	 *
	 * @param resourceIds the new resource ids
	 */
	public void setResourceIds(String resourceIds) {

		this.resourceIds = resourceIds;
	}

	/**
	 * Gets the scope.
	 *
	 * @return the scope
	 */
	public String getScope() {

		return this.scope;
	}

	/**
	 * Sets the scope.
	 *
	 * @param scope the new scope
	 */
	public void setScope(String scope) {

		this.scope = scope;
	}

	/**
	 * Gets the web server redirect uri.
	 *
	 * @return the web server redirect uri
	 */
	public String getWebServerRedirectUri() {

		return this.webServerRedirectUri;
	}

	/**
	 * Sets the web server redirect uri.
	 *
	 * @param webServerRedirectUri the new web server redirect uri
	 */
	public void setWebServerRedirectUri(String webServerRedirectUri) {

		this.webServerRedirectUri = webServerRedirectUri;
	}

}
