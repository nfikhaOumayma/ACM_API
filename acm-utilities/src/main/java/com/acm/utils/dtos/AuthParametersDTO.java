/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class AuthParametersDTO.
 */
public class AuthParametersDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4618452305308151846L;

	/** The user name. */
	@JsonProperty("USERNAME")
	public String USERNAME;

	/** The password. */
	@JsonProperty("PASSWORD")
	public String PASSWORD;

	/** The secret hash. */
	@JsonProperty("SECRET_HASH")
	public String SECRET_HASH;

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUSERNAME() {

		return USERNAME;
	}

	/**
	 * Sets the username.
	 *
	 * @param uSERNAME the new username
	 */
	public void setUSERNAME(String uSERNAME) {

		USERNAME = uSERNAME;
	}

	/**
	 * Gets the password.
	 *
	 * @return the password
	 */
	public String getPASSWORD() {

		return PASSWORD;
	}

	/**
	 * Sets the password.
	 *
	 * @param pASSWORD the new password
	 */
	public void setPASSWORD(String pASSWORD) {

		PASSWORD = pASSWORD;
	}

	/**
	 * Gets the secret hash.
	 *
	 * @return the secret hash
	 */
	public String getSECRET_HASH() {

		return SECRET_HASH;
	}

	/**
	 * Sets the secret hash.
	 *
	 * @param sECRET_HASH the new secret hash
	 */
	public void setSECRET_HASH(String sECRET_HASH) {

		SECRET_HASH = sECRET_HASH;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AuthParametersDTO [USERNAME=" + USERNAME + ", PASSWORD=" + PASSWORD
				+ ", SECRET_HASH=" + SECRET_HASH + "]";
	}

}
