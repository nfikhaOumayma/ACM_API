/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;

/**
 * {@link CustomerAbacusAPIModelOrganisation} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class CustomerAbacusAPIModelOrganisation implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 406960320984242447L;

	/** The organisation ID. */
	private int organisationID;

	/** The reg number. */
	private String regNumber;

	/** The type. */
	private int type;

	/** The telephone 1. */
	private String telephone1;

	/** The telephone 2. */
	private String telephone2;

	/** The fax. */
	private String fax;

	/** The web site. */
	private String webSite;

	/** The e mail. */
	private String eMail;

	/** The accounts year end. */
	private String accountsYearEnd;

	/**
	 * Gets the organisation ID.
	 *
	 * @return the organisationID
	 */
	public int getOrganisationID() {

		return organisationID;
	}

	/**
	 * Sets the organisation ID.
	 *
	 * @param organisationID the organisationID to set
	 */
	public void setOrganisationID(int organisationID) {

		this.organisationID = organisationID;
	}

	/**
	 * Gets the reg number.
	 *
	 * @return the regNumber
	 */
	public String getRegNumber() {

		return regNumber;
	}

	/**
	 * Sets the reg number.
	 *
	 * @param regNumber the regNumber to set
	 */
	public void setRegNumber(String regNumber) {

		this.regNumber = regNumber;
	}

	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public int getType() {

		return type;
	}

	/**
	 * Sets the type.
	 *
	 * @param type the type to set
	 */
	public void setType(int type) {

		this.type = type;
	}

	/**
	 * Gets the telephone 1.
	 *
	 * @return the telephone1
	 */
	public String getTelephone1() {

		return telephone1;
	}

	/**
	 * Sets the telephone 1.
	 *
	 * @param telephone1 the telephone1 to set
	 */
	public void setTelephone1(String telephone1) {

		this.telephone1 = telephone1;
	}

	/**
	 * Gets the telephone 2.
	 *
	 * @return the telephone2
	 */
	public String getTelephone2() {

		return telephone2;
	}

	/**
	 * Sets the telephone 2.
	 *
	 * @param telephone2 the telephone2 to set
	 */
	public void setTelephone2(String telephone2) {

		this.telephone2 = telephone2;
	}

	/**
	 * Gets the fax.
	 *
	 * @return the fax
	 */
	public String getFax() {

		return fax;
	}

	/**
	 * Sets the fax.
	 *
	 * @param fax the fax to set
	 */
	public void setFax(String fax) {

		this.fax = fax;
	}

	/**
	 * Gets the web site.
	 *
	 * @return the webSite
	 */
	public String getWebSite() {

		return webSite;
	}

	/**
	 * Sets the web site.
	 *
	 * @param webSite the webSite to set
	 */
	public void setWebSite(String webSite) {

		this.webSite = webSite;
	}

	/**
	 * Gets the e mail.
	 *
	 * @return the eMail
	 */
	public String geteMail() {

		return eMail;
	}

	/**
	 * Sets the e mail.
	 *
	 * @param eMail the eMail to set
	 */
	public void seteMail(String eMail) {

		this.eMail = eMail;
	}

	/**
	 * Gets the accounts year end.
	 *
	 * @return the accountsYearEnd
	 */
	public String getAccountsYearEnd() {

		return accountsYearEnd;
	}

	/**
	 * Sets the accounts year end.
	 *
	 * @param accountsYearEnd the accountsYearEnd to set
	 */
	public void setAccountsYearEnd(String accountsYearEnd) {

		this.accountsYearEnd = accountsYearEnd;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelOrganisation [organisationID=" + organisationID
				+ ", regNumber=" + regNumber + ", type=" + type + ", telephone1=" + telephone1
				+ ", telephone2=" + telephone2 + ", fax=" + fax + ", webSite=" + webSite
				+ ", eMail=" + eMail + ", accountsYearEnd=" + accountsYearEnd + "]";
	}

}
