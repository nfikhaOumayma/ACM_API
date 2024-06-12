/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class AuthResponseMurabhaApiDTO.
 */
public class AuthResponseMurabhaApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -9075273164168640452L;

	/** The authentication result. */
	@JsonProperty("AuthenticationResult")
	public AuthenticationResultDTO authenticationResult;

	/** The challenge parameters. */
	@JsonProperty("ChallengeParameters")
	private ChallengeParametersDTO challengeParameters = new ChallengeParametersDTO();

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AuthResponseMurabhaApiDTO [authenticationResult=" + authenticationResult
				+ ", challengeParameters=" + challengeParameters + "]";
	}

	/**
	 * Gets the authentication result.
	 *
	 * @return the authentication result
	 */
	public AuthenticationResultDTO getAuthenticationResult() {

		return authenticationResult;
	}

	/**
	 * Sets the authentication result.
	 *
	 * @param authenticationResult the new authentication result
	 */
	public void setAuthenticationResult(AuthenticationResultDTO authenticationResult) {

		this.authenticationResult = authenticationResult;
	}

	/**
	 * Gets the challenge parameters.
	 *
	 * @return the challenge parameters
	 */
	public ChallengeParametersDTO getChallengeParameters() {

		return challengeParameters;
	}

	/**
	 * Sets the challenge parameters.
	 *
	 * @param challengeParameters the new challenge parameters
	 */
	public void setChallengeParameters(ChallengeParametersDTO challengeParameters) {

		this.challengeParameters = challengeParameters;
	}

}
