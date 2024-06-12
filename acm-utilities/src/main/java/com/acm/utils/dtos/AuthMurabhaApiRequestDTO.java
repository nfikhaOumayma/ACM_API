/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class AuthMurabhaApiRequestDTO.
 */
public class AuthMurabhaApiRequestDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5437399580505320882L;

	/** The auth parameters. */
	@JsonProperty("AuthParameters")
	private AuthParametersDTO authParameters;

	/** The auth flow. */
	@JsonProperty("AuthFlow")
	private String authFlow;

	/** The client id. */
	@JsonProperty("ClientId")
	private String clientId;

	/**
	 * Gets the auth parameters.
	 *
	 * @return the auth parameters
	 */
	public AuthParametersDTO getAuthParameters() {

		return authParameters;
	}

	/**
	 * Sets the auth parameters.
	 *
	 * @param authParameters the new auth parameters
	 */
	public void setAuthParameters(AuthParametersDTO authParameters) {

		this.authParameters = authParameters;
	}

	/**
	 * Gets the auth flow.
	 *
	 * @return the auth flow
	 */
	public String getAuthFlow() {

		return authFlow;
	}

	/**
	 * Sets the auth flow.
	 *
	 * @param authFlow the new auth flow
	 */
	public void setAuthFlow(String authFlow) {

		this.authFlow = authFlow;
	}

	/**
	 * Gets the client id.
	 *
	 * @return the client id
	 */
	public String getClientId() {

		return clientId;
	}

	/**
	 * Sets the client id.
	 *
	 * @param clientId the new client id
	 */
	public void setClientId(String clientId) {

		this.clientId = clientId;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AuthMurabhaApiRequestDTO [authParameters=" + authParameters + ", authFlow="
				+ authFlow + ", clientId=" + clientId + "]";
	}

}
