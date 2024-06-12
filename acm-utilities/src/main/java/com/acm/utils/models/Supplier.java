/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * {@link Supplier} class.
 *
 * @author KhaledOuali
 * @since 1.12
 */
@Entity
@Table(name = "ACM_SUPPLIER")
@NamedQuery(name = "Supplier.findAll", query = "SELECT s FROM Supplier s")
public class Supplier extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1027544933499745315L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SUPPLIER", unique = true, nullable = false)
	private Long id;

	/** The type. */
	@Column(name = "TYPE_ID")
	private Long type;

	/** The name. */
	@Column(name = "NAME")
	private String name;

	/** The activity. */
	@Column(name = "ACTIVITY")
	private Long activity;

	/** The aronyme . */
	@Column(name = "ACRONYME")
	private String acronyme;

	/** The legal catalog. */
	@Column(name = "LEGAL_CATALOG")
	private Long legalCatalog;

	/** The id account extern. */
	@Column(name = "COMMERCIAL_NAME")
	private String commercialName;

	/** The email. */
	@Column(name = "EMAIL")
	private String email;

	/** The activity Start Date. */
	@JoinColumn(name = "ACTIVITY_START_DATE")
	private Date activityStartDate;

	/** The register number. */
	@Column(name = "REGISTER_NUMBER", unique = true)
	private String registerNumber;

	/** The telephone. */
	@Column(name = "TELEPHONE")
	private String telephone;

	/** The telephone 2. */
	@Column(name = "TELEPHONE_2")
	private String telephone2;

	/** The web site. */
	@Column(name = "WEB_SITE")
	private String webSite;

	/** The web site. */
	@Column(name = "STATUS")
	private String status;

	/** The periodicity. */
	@Column(name = "PERIODICITY")
	private Long periodicity;

	/** The objectif. */
	@Column(name = "OBJECTIF")
	private Long objectif;

	/** The identity. */
	@Column(name = "IDENTITY_NUMBER", unique = true)
	private String identity;

	/** The is customer. */
	@Column(name = "IS_CUSTOMER")
	private Boolean isCustomer;

	/** The conventions. */
	@JsonIgnore
	@OneToMany(mappedBy = "supplier", fetch = FetchType.LAZY, orphanRemoval = true)
	private Set<Convention> conventions = new HashSet<>();

	/** The address. */
	@JsonIgnore
	@OneToMany(mappedBy = "supplier", fetch = FetchType.LAZY)
	private List<Address> listAddress;

	/** The activity name. */
	@Column(name = "ACTIVITY_NAME")
	private String activityName;

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
	public Set<Convention> getConventions() {

		return conventions;
	}

	/**
	 * Sets the conventions.
	 *
	 * @param conventions the new conventions
	 */
	public void setConventions(Set<Convention> conventions) {

		this.conventions = conventions;
	}

	/**
	 * Gets the list address.
	 *
	 * @return the list address
	 */
	public List<Address> getListAddress() {

		return listAddress;
	}

	/**
	 * Sets the list address.
	 *
	 * @param listAddress the new list address
	 */
	public void setListAddress(List<Address> listAddress) {

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

}
