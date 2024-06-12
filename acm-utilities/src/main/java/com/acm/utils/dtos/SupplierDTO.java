/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonManagedReference;

/**
 * The {@link Supplier} class.
 * 
 * @author KhaledOuali
 * @since 1.12
 */
public class SupplierDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The id. */

	private Long id;

	/** The type. */
	private Long type;

	/** The name. */
	private String name;

	/** The activity. */
	private Long activity;

	/** The aronyme . */
	private String acronyme;

	/** The legal catalog. */
	private Long legalCatalog;

	/** The id account extern. */
	private String commercialName;

	/** The email. */
	private String email;

	/** The activity Start Date. */
	private Date activityStartDate;

	/** The register number. */
	private String registerNumber;

	/** The telephone. */
	private String telephone;

	/** The telephone 2. */
	private String telephone2;

	/** The web site. */
	private String webSite;

	/** The status. */
	private String status; // NON CONTRACTED ; REJECTED ; APPROVED ; POTENTIEL

	/** The periodicity. */
	private Long periodicity;

	/** The objectif. */
	private Long objectif;

	/** The conventions. */
	private Set<ConventionDTO> conventions = new HashSet<>();

	/** The address DT os. */
	@JsonManagedReference
	private List<AddressDTO> listAddress;

	/** The insert by. */
	private String insertBy;

	/** The date insertion. */
	private Date dateInsertion;

	/** The balance supplier. */
	private BigDecimal balanceSupplier;

	/** The status not contracted. */
	private String statusNotContracted;

	/** The status rejected. */
	private String statusRejected;

	/** The activity name. */
	private String activityName;

	/** The identity. */
	private String identity;

	/** The is customer. */
	private Boolean isCustomer;

	/** The user defined fields links DT os. */
	private List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public Long getType() {

		return type;
	}

	/**
	 * Sets the type.
	 *
	 * @param type the new type
	 */
	public void setType(Long type) {

		this.type = type;
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
	 * @param name the new name
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the activity.
	 *
	 * @return the activity
	 */
	public Long getActivity() {

		return activity;
	}

	/**
	 * Sets the activity.
	 *
	 * @param activity the new activity
	 */
	public void setActivity(Long activity) {

		this.activity = activity;
	}

	/**
	 * Gets the acronyme.
	 *
	 * @return the acronyme
	 */
	public String getAcronyme() {

		return acronyme;
	}

	/**
	 * Sets the acronyme.
	 *
	 * @param acronyme the new acronyme
	 */
	public void setAcronyme(String acronyme) {

		this.acronyme = acronyme;
	}

	/**
	 * Gets the legal catalog.
	 *
	 * @return the legal catalog
	 */
	public Long getLegalCatalog() {

		return legalCatalog;
	}

	/**
	 * Sets the legal catalog.
	 *
	 * @param legalCatalog the new legal catalog
	 */
	public void setLegalCatalog(Long legalCatalog) {

		this.legalCatalog = legalCatalog;
	}

	/**
	 * Gets the commercial name.
	 *
	 * @return the commercial name
	 */
	public String getCommercialName() {

		return commercialName;
	}

	/**
	 * Sets the commercial name.
	 *
	 * @param commercialName the new commercial name
	 */
	public void setCommercialName(String commercialName) {

		this.commercialName = commercialName;
	}

	/**
	 * Gets the email.
	 *
	 * @return the email
	 */
	public String getEmail() {

		return email;
	}

	/**
	 * Sets the email.
	 *
	 * @param email the new email
	 */
	public void setEmail(String email) {

		this.email = email;
	}

	/**
	 * Gets the activity start date.
	 *
	 * @return the activity start date
	 */
	public Date getActivityStartDate() {

		return activityStartDate;
	}

	/**
	 * Sets the activity start date.
	 *
	 * @param activityStartDate the new activity start date
	 */
	public void setActivityStartDate(Date activityStartDate) {

		this.activityStartDate = activityStartDate;
	}

	/**
	 * Gets the telephone.
	 *
	 * @return the telephone
	 */
	public String getTelephone() {

		return telephone;
	}

	/**
	 * Sets the telephone.
	 *
	 * @param telephone the new telephone
	 */
	public void setTelephone(String telephone) {

		this.telephone = telephone;
	}

	/**
	 * Gets the telephone 2.
	 *
	 * @return the telephone 2
	 */
	public String getTelephone2() {

		return telephone2;
	}

	/**
	 * Sets the telephone 2.
	 *
	 * @param telephone2 the new telephone 2
	 */
	public void setTelephone2(String telephone2) {

		this.telephone2 = telephone2;
	}

	/**
	 * Gets the web site.
	 *
	 * @return the web site
	 */
	public String getWebSite() {

		return webSite;
	}

	/**
	 * Sets the web site.
	 *
	 * @param webSite the new web site
	 */
	public void setWebSite(String webSite) {

		this.webSite = webSite;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public String getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the status to set
	 */
	public void setStatus(String status) {

		this.status = status;
	}

	/**
	 * Gets the periodicity.
	 *
	 * @return the periodicity
	 */
	public Long getPeriodicity() {

		return periodicity;
	}

	/**
	 * Sets the periodicity.
	 *
	 * @param periodicity the new periodicity
	 */
	public void setPeriodicity(Long periodicity) {

		this.periodicity = periodicity;
	}

	/**
	 * Gets the conventions.
	 *
	 * @return the conventions
	 */
	public Set<ConventionDTO> getConventions() {

		return conventions;
	}

	/**
	 * Sets the conventions.
	 *
	 * @param conventions the new conventions
	 */
	public void setConventions(Set<ConventionDTO> conventions) {

		this.conventions = conventions;
	}

	/**
	 * Gets the list address.
	 *
	 * @return the list address
	 */
	public List<AddressDTO> getListAddress() {

		return listAddress;
	}

	/**
	 * Sets the list address.
	 *
	 * @param listAddress the new list address
	 */
	public void setListAddress(List<AddressDTO> listAddress) {

		this.listAddress = listAddress;
	}

	/**
	 * Gets the objectif.
	 *
	 * @return the objectif
	 */
	public Long getObjectif() {

		return objectif;
	}

	/**
	 * Sets the objectif.
	 *
	 * @param objectif the new objectif
	 */
	public void setObjectif(Long objectif) {

		this.objectif = objectif;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insert by
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the new insert by
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the balance supplier.
	 *
	 * @return the balance supplier
	 */
	public BigDecimal getBalanceSupplier() {

		return balanceSupplier;
	}

	/**
	 * Sets the balance supplier.
	 *
	 * @param balanceSupplier the new balance supplier
	 */
	public void setBalanceSupplier(BigDecimal balanceSupplier) {

		this.balanceSupplier = balanceSupplier;
	}

	/**
	 * Gets the status not contracted.
	 *
	 * @return the statusNotContracted
	 */
	public String getStatusNotContracted() {

		return statusNotContracted;
	}

	/**
	 * Sets the status not contracted.
	 *
	 * @param statusNotContracted the statusNotContracted to set
	 */
	public void setStatusNotContracted(String statusNotContracted) {

		this.statusNotContracted = statusNotContracted;
	}

	/**
	 * Gets the status rejected.
	 *
	 * @return the statusRejected
	 */
	public String getStatusRejected() {

		return statusRejected;
	}

	/**
	 * Sets the status rejected.
	 *
	 * @param statusRejected the statusRejected to set
	 */
	public void setStatusRejected(String statusRejected) {

		this.statusRejected = statusRejected;
	}

	/**
	 * Gets the activity name.
	 *
	 * @return the activity name
	 */
	public String getActivityName() {

		return activityName;
	}

	/**
	 * Sets the activity name.
	 *
	 * @param activityName the new activity name
	 */
	public void setActivityName(String activityName) {

		this.activityName = activityName;
	}

	/**
	 * Gets the user defined fields links DT os.
	 *
	 * @return the user defined fields links DT os
	 */
	public List<UserDefinedFieldsLinksDTO> getUserDefinedFieldsLinksDTOs() {

		return userDefinedFieldsLinksDTOs;
	}

	/**
	 * Sets the user defined fields links DT os.
	 *
	 * @param userDefinedFieldsLinksDTOs the new user defined fields links DT os
	 */
	public void setUserDefinedFieldsLinksDTOs(
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs) {

		this.userDefinedFieldsLinksDTOs = userDefinedFieldsLinksDTOs;
	}

	/**
	 * Gets the identity.
	 *
	 * @return the identity
	 */
	public String getIdentity() {

		return identity;
	}

	/**
	 * Sets the identity.
	 *
	 * @param identity the new identity
	 */
	public void setIdentity(String identity) {

		this.identity = identity;
	}

	/**
	 * Gets the checks if is customer.
	 *
	 * @return the checks if is customer
	 */
	public Boolean getIsCustomer() {

		return isCustomer;
	}

	/**
	 * Sets the checks if is customer.
	 *
	 * @param isCustomer the new checks if is customer
	 */
	public void setIsCustomer(Boolean isCustomer) {

		this.isCustomer = isCustomer;
	}

	/**
	 * Gets the register number.
	 *
	 * @return the register number
	 */
	public String getRegisterNumber() {

		return registerNumber;
	}

	/**
	 * Sets the register number.
	 *
	 * @param registerNumber the new register number
	 */
	public void setRegisterNumber(String registerNumber) {

		this.registerNumber = registerNumber;
	}

}
