/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link BrancheDTO} class.
 *
 * @author YesserSomai
 * @since 1.0.2
 */
public class BrancheDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8509499829978144246L;

	/** The branch ID. */
	private Long branchID;
	/** The name. */
	private String name;
	/** The description. */
	private String description;

	/** The code. */
	private String code;

	/** The sort code. */
	private String sortCode;

	/** The access all. */
	private Boolean accessAll;

	/** The allow customers. */
	private Boolean allowCustomers;

	/** The back office GL. */
	private Boolean backOfficeGL;

	/** The enabled. */
	private Boolean enabled;

	/** The address ID. */
	private Long addressID;

	/** The branch phone number. */
	private String branchPhoneNumber;

	/** The branch fax number. */
	private String branchFaxNumber;

	/** The branch email. */
	private String branchEmail;

	/** The increment days. */
	private Integer incrementDays;

	/** The use closed days holidays. */
	private Integer useClosedDaysHolidays;

	/** The closed days. */
	private Integer closedDays;

	/** The cheque clearance personal. */
	private Integer chequeClearancePersonal;

	/** The cheque clearance third party. */
	private Integer chequeClearanceThirdParty;

	/** The parent branch ID. */
	private Long parentBranchID;

	/** The regional manager ID. */
	private Long regionalManagerID;

	/** The g L account code. */
	private String gLAccountCode;

	/**
	 * Instantiates a new branche DTO.
	 */
	public BrancheDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new branche DTO.
	 *
	 * @param branchID the branch ID
	 * @param name the name
	 * @param description the description
	 * @param code the code
	 * @param sortCode the sort code
	 * @param accessAll the access all
	 * @param allowCustomers the allow customers
	 * @param backOfficeGL the back office GL
	 * @param enabled the enabled
	 * @param addressID the address ID
	 * @param branchPhoneNumber the branch phone number
	 * @param branchFaxNumber the branch fax number
	 * @param branchEmail the branch email
	 * @param incrementDays the increment days
	 * @param useClosedDaysHolidays the use closed days holidays
	 * @param closedDays the closed days
	 * @param chequeClearancePersonal the cheque clearance personal
	 * @param chequeClearanceThirdParty the cheque clearance third party
	 * @param parentBranchID the parent branch ID
	 * @param regionalManagerID the regional manager ID
	 * @param gLAccountCode the g L account code
	 */
	public BrancheDTO(Long branchID, String name, String description, String code, String sortCode,
			Boolean accessAll, Boolean allowCustomers, Boolean backOfficeGL, Boolean enabled,
			Long addressID, String branchPhoneNumber, String branchFaxNumber, String branchEmail,
			Integer incrementDays, Integer useClosedDaysHolidays, Integer closedDays,
			Integer chequeClearancePersonal, Integer chequeClearanceThirdParty, Long parentBranchID,
			Long regionalManagerID, String gLAccountCode) {

		super();
		this.branchID = branchID;
		this.name = name;
		this.description = description;
		this.code = code;
		this.sortCode = sortCode;
		this.accessAll = accessAll;
		this.allowCustomers = allowCustomers;
		this.backOfficeGL = backOfficeGL;
		this.enabled = enabled;
		this.addressID = addressID;
		this.branchPhoneNumber = branchPhoneNumber;
		this.branchFaxNumber = branchFaxNumber;
		this.branchEmail = branchEmail;
		this.incrementDays = incrementDays;
		this.useClosedDaysHolidays = useClosedDaysHolidays;
		this.closedDays = closedDays;
		this.chequeClearancePersonal = chequeClearancePersonal;
		this.chequeClearanceThirdParty = chequeClearanceThirdParty;
		this.parentBranchID = parentBranchID;
		this.regionalManagerID = regionalManagerID;
		this.gLAccountCode = gLAccountCode;
	}

	/**
	 * Gets the branch ID.
	 *
	 * @return the branchID
	 */
	public Long getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 *
	 * @param branchID the branchID to set
	 */
	public void setBranchID(Long branchID) {

		this.branchID = branchID;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the sort code.
	 *
	 * @return the sortCode
	 */
	public String getSortCode() {

		return sortCode;
	}

	/**
	 * Sets the sort code.
	 *
	 * @param sortCode the sortCode to set
	 */
	public void setSortCode(String sortCode) {

		this.sortCode = sortCode;
	}

	/**
	 * Gets the access all.
	 *
	 * @return the accessAll
	 */
	public Boolean getAccessAll() {

		return accessAll;
	}

	/**
	 * Sets the access all.
	 *
	 * @param accessAll the accessAll to set
	 */
	public void setAccessAll(Boolean accessAll) {

		this.accessAll = accessAll;
	}

	/**
	 * Gets the allow customers.
	 *
	 * @return the allowCustomers
	 */
	public Boolean getAllowCustomers() {

		return allowCustomers;
	}

	/**
	 * Sets the allow customers.
	 *
	 * @param allowCustomers the allowCustomers to set
	 */
	public void setAllowCustomers(Boolean allowCustomers) {

		this.allowCustomers = allowCustomers;
	}

	/**
	 * Gets the back office GL.
	 *
	 * @return the backOfficeGL
	 */
	public Boolean getBackOfficeGL() {

		return backOfficeGL;
	}

	/**
	 * Sets the back office GL.
	 *
	 * @param backOfficeGL the backOfficeGL to set
	 */
	public void setBackOfficeGL(Boolean backOfficeGL) {

		this.backOfficeGL = backOfficeGL;
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the address ID.
	 *
	 * @return the addressID
	 */
	public Long getAddressID() {

		return addressID;
	}

	/**
	 * Sets the address ID.
	 *
	 * @param addressID the addressID to set
	 */
	public void setAddressID(Long addressID) {

		this.addressID = addressID;
	}

	/**
	 * Gets the branch phone number.
	 *
	 * @return the branchPhoneNumber
	 */
	public String getBranchPhoneNumber() {

		return branchPhoneNumber;
	}

	/**
	 * Sets the branch phone number.
	 *
	 * @param branchPhoneNumber the branchPhoneNumber to set
	 */
	public void setBranchPhoneNumber(String branchPhoneNumber) {

		this.branchPhoneNumber = branchPhoneNumber;
	}

	/**
	 * Gets the branch fax number.
	 *
	 * @return the branchFaxNumber
	 */
	public String getBranchFaxNumber() {

		return branchFaxNumber;
	}

	/**
	 * Sets the branch fax number.
	 *
	 * @param branchFaxNumber the branchFaxNumber to set
	 */
	public void setBranchFaxNumber(String branchFaxNumber) {

		this.branchFaxNumber = branchFaxNumber;
	}

	/**
	 * Gets the branch email.
	 *
	 * @return the branchEmail
	 */
	public String getBranchEmail() {

		return branchEmail;
	}

	/**
	 * Sets the branch email.
	 *
	 * @param branchEmail the branchEmail to set
	 */
	public void setBranchEmail(String branchEmail) {

		this.branchEmail = branchEmail;
	}

	/**
	 * Gets the increment days.
	 *
	 * @return the incrementDays
	 */
	public Integer getIncrementDays() {

		return incrementDays;
	}

	/**
	 * Sets the increment days.
	 *
	 * @param incrementDays the incrementDays to set
	 */
	public void setIncrementDays(Integer incrementDays) {

		this.incrementDays = incrementDays;
	}

	/**
	 * Gets the use closed days holidays.
	 *
	 * @return the useClosedDaysHolidays
	 */
	public Integer getUseClosedDaysHolidays() {

		return useClosedDaysHolidays;
	}

	/**
	 * Sets the use closed days holidays.
	 *
	 * @param useClosedDaysHolidays the useClosedDaysHolidays to set
	 */
	public void setUseClosedDaysHolidays(Integer useClosedDaysHolidays) {

		this.useClosedDaysHolidays = useClosedDaysHolidays;
	}

	/**
	 * Gets the closed days.
	 *
	 * @return the closedDays
	 */
	public Integer getClosedDays() {

		return closedDays;
	}

	/**
	 * Sets the closed days.
	 *
	 * @param closedDays the closedDays to set
	 */
	public void setClosedDays(Integer closedDays) {

		this.closedDays = closedDays;
	}

	/**
	 * Gets the cheque clearance personal.
	 *
	 * @return the chequeClearancePersonal
	 */
	public Integer getChequeClearancePersonal() {

		return chequeClearancePersonal;
	}

	/**
	 * Sets the cheque clearance personal.
	 *
	 * @param chequeClearancePersonal the chequeClearancePersonal to set
	 */
	public void setChequeClearancePersonal(Integer chequeClearancePersonal) {

		this.chequeClearancePersonal = chequeClearancePersonal;
	}

	/**
	 * Gets the cheque clearance third party.
	 *
	 * @return the chequeClearanceThirdParty
	 */
	public Integer getChequeClearanceThirdParty() {

		return chequeClearanceThirdParty;
	}

	/**
	 * Sets the cheque clearance third party.
	 *
	 * @param chequeClearanceThirdParty the chequeClearanceThirdParty to set
	 */
	public void setChequeClearanceThirdParty(Integer chequeClearanceThirdParty) {

		this.chequeClearanceThirdParty = chequeClearanceThirdParty;
	}

	/**
	 * Gets the parent branch ID.
	 *
	 * @return the parentBranchID
	 */
	public Long getParentBranchID() {

		return parentBranchID;
	}

	/**
	 * Sets the parent branch ID.
	 *
	 * @param parentBranchID the parentBranchID to set
	 */
	public void setParentBranchID(Long parentBranchID) {

		this.parentBranchID = parentBranchID;
	}

	/**
	 * Gets the regional manager ID.
	 *
	 * @return the regionalManagerID
	 */
	public Long getRegionalManagerID() {

		return regionalManagerID;
	}

	/**
	 * Sets the regional manager ID.
	 *
	 * @param regionalManagerID the regionalManagerID to set
	 */
	public void setRegionalManagerID(Long regionalManagerID) {

		this.regionalManagerID = regionalManagerID;
	}

	/**
	 * Gets the g L account code.
	 *
	 * @return the gLAccountCode
	 */
	public String getgLAccountCode() {

		return gLAccountCode;
	}

	/**
	 * Sets the g L account code.
	 *
	 * @param gLAccountCode the gLAccountCode to set
	 */
	public void setgLAccountCode(String gLAccountCode) {

		this.gLAccountCode = gLAccountCode;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "BrancheDTO [branchID=" + branchID + ", name=" + name + ", description="
				+ description + ", code=" + code + ", sortCode=" + sortCode + ", accessAll="
				+ accessAll + ", allowCustomers=" + allowCustomers + ", backOfficeGL="
				+ backOfficeGL + ", enabled=" + enabled + ", addressID=" + addressID
				+ ", branchPhoneNumber=" + branchPhoneNumber + ", branchFaxNumber="
				+ branchFaxNumber + ", branchEmail=" + branchEmail + ", incrementDays="
				+ incrementDays + ", useClosedDaysHolidays=" + useClosedDaysHolidays
				+ ", closedDays=" + closedDays + ", chequeClearancePersonal="
				+ chequeClearancePersonal + ", chequeClearanceThirdParty="
				+ chequeClearanceThirdParty + ", parentBranchID=" + parentBranchID
				+ ", regionalManagerID=" + regionalManagerID + ", gLAccountCode=" + gLAccountCode
				+ "]";
	}

}
