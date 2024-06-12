/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class RequestApplicantSimahApiDTO.
 */
public class RequestApplicantSimahApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 636460985176685683L;

	/** The identity info. */
	@JsonProperty("identityInfo")
	private IdentityInfoDTO identityInfo;

	/** The demographic info. */
	@JsonProperty("demographicInfo")
	private DemographicInfoRequestSimahApiDTO demographicInfo;

	/** The address. */
	@JsonProperty("address")
	private RequestAddressSimahApiDTO address;

	/** The contact. */
	@JsonProperty("contact")
	private ContactRequestSimahApiDTO contact;

	/** The occupation. */
	@JsonProperty("occupation")
	private OccupationRequestSimahApiDTO occupation;

	/**
	 * Instantiates a new request applicant simah api DTO.
	 */
	public RequestApplicantSimahApiDTO() {

		super();
	}

	/**
	 * Gets the identity info.
	 *
	 * @return the identity info
	 */
	public IdentityInfoDTO getIdentityInfo() {

		return identityInfo;
	}

	/**
	 * Sets the identity info.
	 *
	 * @param identityInfo the new identity info
	 */
	public void setIdentityInfo(IdentityInfoDTO identityInfo) {

		this.identityInfo = identityInfo;
	}

	/**
	 * Gets the demographic info.
	 *
	 * @return the demographic info
	 */
	public DemographicInfoRequestSimahApiDTO getDemographicInfo() {

		return demographicInfo;
	}

	/**
	 * Sets the demographic info.
	 *
	 * @param demographicInfo the new demographic info
	 */
	public void setDemographicInfo(DemographicInfoRequestSimahApiDTO demographicInfo) {

		this.demographicInfo = demographicInfo;
	}

	/**
	 * Gets the address.
	 *
	 * @return the address
	 */
	public RequestAddressSimahApiDTO getAddress() {

		return address;
	}

	/**
	 * Sets the address.
	 *
	 * @param address the new address
	 */
	public void setAddress(RequestAddressSimahApiDTO address) {

		this.address = address;
	}

	/**
	 * Gets the contact.
	 *
	 * @return the contact
	 */
	public ContactRequestSimahApiDTO getContact() {

		return contact;
	}

	/**
	 * Sets the contact.
	 *
	 * @param contact the new contact
	 */
	public void setContact(ContactRequestSimahApiDTO contact) {

		this.contact = contact;
	}

	/**
	 * Gets the occupation.
	 *
	 * @return the occupation
	 */
	public OccupationRequestSimahApiDTO getOccupation() {

		return occupation;
	}

	/**
	 * Sets the occupation.
	 *
	 * @param occupation the new occupation
	 */
	public void setOccupation(OccupationRequestSimahApiDTO occupation) {

		this.occupation = occupation;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestApplicantSimahApiDTO [identityInfo=" + identityInfo + ", demographicInfo="
				+ demographicInfo + ", address=" + address + ", contact=" + contact
				+ ", occupation=" + occupation + "]";
	}

}
