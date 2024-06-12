/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.acm.utils.audit.AuditTrailListener;

/**
 * The persistent class for the ACM_ADDRESS database table. {@link Address} class.
 * 
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_ADDRESS")
@EntityListeners(AuditTrailListener.class)
public class Address extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2053186227049225170L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_ADDRESS", unique = true, nullable = false)
	private Long id;

	/** The id address abacus. */
	@Column(name = "ID_ABACUS_ADDRESS")
	private Long idAddressAbacus;

	/** The customer id. */
	@Column(name = "CUSTOMER_ID", nullable = false)
	private Long customerId;

	/** The address 1. */
	@Column(name = "ADDRESS_1", length = 512)
	private String address1;

	/** The address 2. */
	@Column(name = "ADDRESS_2", length = 512)
	private String address2;

	/** The address 3. */
	@Column(name = "ADDRESS_3", length = 512)
	private String address3;

	/** The town city. */
	@Column(name = "TOWN_CITY", length = 512)
	private String townCity;

	/** The county. */
	@Column(name = "COUNTY", length = 512)
	private String county;

	/** The state. */
	@Column(name = "STATE", length = 512)
	private String state;

	/** The postal code. */
	@Column(name = "POSTAL_CODE", length = 256)
	private String postalCode;

	/** The country. */
	@Column(name = "COUNTRY", length = 512)
	private String country;

	/** The region. */
	@Column(name = "REGION", length = 512)
	private String region;

	/** The address 1 id. */
	@Column(name = "ADDRESS1_ID")
	private Long address1Id;

	/** The address 2 id. */
	@Column(name = "ADDRESS2_ID")
	private Long address2Id;

	/** The address 3 id. */
	@Column(name = "ADDRESS3_ID")
	private Long address3Id;

	/** The town city id. */
	@Column(name = "TOWN_CITY_ID")
	private Long townCityId;

	/** The county id. */
	@Column(name = "COUNTY_ID")
	private Long countyId;

	/** The state id. */
	@Column(name = "STATE_ID")
	private Long stateId;

	/** The postal code id. */
	@Column(name = "POSTAL_CODE_ID")
	private Long postalCodeId;

	/** The country id. */
	@Column(name = "COUNTRY_ID")
	private Long countryId;

	/** The region id. */
	@Column(name = "REGION_ID")
	private Long regionId;

	/** The address type id. */
	@Column(name = "ADDRESS_TYPE_ID")
	private Long addressTypeId;

	/** The date moved in. */
	@Column(name = "DATE_MOVED_IN")
	private Date dateMovedIn;

	/** The date moved out. */
	@Column(name = "DATE_MOVED_OUT")
	private Date dateMovedOut;

	/** The is primary. */
	@Column(name = "IS_PRIMARY", nullable = false)
	private Boolean isPrimary;

	/** The latitude . */
	@Column(name = "LATITUDE")
	private String lan;

	/** The longitude. */
	@Column(name = "LONGITUDE")
	private String lng;

	/** The id supplier. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_SUPPLIER")
	private Supplier supplier;

	/** The token. */
	@Transient
	private String token;

	/**
	 * Instantiates a new address.
	 */
	public Address() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new address.
	 *
	 * @param id the id
	 */
	public Address(Long id) {

		this.id = id;
	}

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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the address 1.
	 *
	 * @return the address1
	 */
	public String getAddress1() {

		return address1;
	}

	/**
	 * Sets the address 1.
	 *
	 * @param address1 the address1 to set
	 */
	public void setAddress1(String address1) {

		this.address1 = address1;
	}

	/**
	 * Gets the address 2.
	 *
	 * @return the address2
	 */
	public String getAddress2() {

		return address2;
	}

	/**
	 * Sets the address 2.
	 *
	 * @param address2 the address2 to set
	 */
	public void setAddress2(String address2) {

		this.address2 = address2;
	}

	/**
	 * Gets the address 3.
	 *
	 * @return the address3
	 */
	public String getAddress3() {

		return address3;
	}

	/**
	 * Sets the address 3.
	 *
	 * @param address3 the address3 to set
	 */
	public void setAddress3(String address3) {

		this.address3 = address3;
	}

	/**
	 * Gets the town city.
	 *
	 * @return the townCity
	 */
	public String getTownCity() {

		return townCity;
	}

	/**
	 * Sets the town city.
	 *
	 * @param townCity the townCity to set
	 */
	public void setTownCity(String townCity) {

		this.townCity = townCity;
	}

	/**
	 * Gets the county.
	 *
	 * @return the county
	 */
	public String getCounty() {

		return county;
	}

	/**
	 * Sets the county.
	 *
	 * @param county the county to set
	 */
	public void setCounty(String county) {

		this.county = county;
	}

	/**
	 * Gets the state.
	 *
	 * @return the state
	 */
	public String getState() {

		return state;
	}

	/**
	 * Sets the state.
	 *
	 * @param state the state to set
	 */
	public void setState(String state) {

		this.state = state;
	}

	/**
	 * Gets the postal code.
	 *
	 * @return the postalCode
	 */
	public String getPostalCode() {

		return postalCode;
	}

	/**
	 * Sets the postal code.
	 *
	 * @param postalCode the postalCode to set
	 */
	public void setPostalCode(String postalCode) {

		this.postalCode = postalCode;
	}

	/**
	 * Gets the country.
	 *
	 * @return the country
	 */
	public String getCountry() {

		return country;
	}

	/**
	 * Sets the country.
	 *
	 * @param country the country to set
	 */
	public void setCountry(String country) {

		this.country = country;
	}

	/**
	 * Gets the region.
	 *
	 * @return the region
	 */
	public String getRegion() {

		return region;
	}

	/**
	 * Sets the region.
	 *
	 * @param region the region to set
	 */
	public void setRegion(String region) {

		this.region = region;
	}

	/**
	 * Gets the address 1 id.
	 *
	 * @return the address1Id
	 */
	public Long getAddress1Id() {

		return address1Id;
	}

	/**
	 * Sets the address 1 id.
	 *
	 * @param address1Id the address1Id to set
	 */
	public void setAddress1Id(Long address1Id) {

		this.address1Id = address1Id;
	}

	/**
	 * Gets the address 2 id.
	 *
	 * @return the address2Id
	 */
	public Long getAddress2Id() {

		return address2Id;
	}

	/**
	 * Sets the address 2 id.
	 *
	 * @param address2Id the address2Id to set
	 */
	public void setAddress2Id(Long address2Id) {

		this.address2Id = address2Id;
	}

	/**
	 * Gets the address 3 id.
	 *
	 * @return the address3Id
	 */
	public Long getAddress3Id() {

		return address3Id;
	}

	/**
	 * Sets the address 3 id.
	 *
	 * @param address3Id the address3Id to set
	 */
	public void setAddress3Id(Long address3Id) {

		this.address3Id = address3Id;
	}

	/**
	 * Gets the town city id.
	 *
	 * @return the townCityId
	 */
	public Long getTownCityId() {

		return townCityId;
	}

	/**
	 * Sets the town city id.
	 *
	 * @param townCityId the townCityId to set
	 */
	public void setTownCityId(Long townCityId) {

		this.townCityId = townCityId;
	}

	/**
	 * Gets the county id.
	 *
	 * @return the countyId
	 */
	public Long getCountyId() {

		return countyId;
	}

	/**
	 * Sets the county id.
	 *
	 * @param countyId the countyId to set
	 */
	public void setCountyId(Long countyId) {

		this.countyId = countyId;
	}

	/**
	 * Gets the state id.
	 *
	 * @return the stateId
	 */
	public Long getStateId() {

		return stateId;
	}

	/**
	 * Sets the state id.
	 *
	 * @param stateId the stateId to set
	 */
	public void setStateId(Long stateId) {

		this.stateId = stateId;
	}

	/**
	 * Gets the postal code id.
	 *
	 * @return the postalCodeId
	 */
	public Long getPostalCodeId() {

		return postalCodeId;
	}

	/**
	 * Sets the postal code id.
	 *
	 * @param postalCodeId the postalCodeId to set
	 */
	public void setPostalCodeId(Long postalCodeId) {

		this.postalCodeId = postalCodeId;
	}

	/**
	 * Gets the country id.
	 *
	 * @return the countryId
	 */
	public Long getCountryId() {

		return countryId;
	}

	/**
	 * Sets the country id.
	 *
	 * @param countryId the countryId to set
	 */
	public void setCountryId(Long countryId) {

		this.countryId = countryId;
	}

	/**
	 * Gets the region id.
	 *
	 * @return the regionId
	 */
	public Long getRegionId() {

		return regionId;
	}

	/**
	 * Sets the region id.
	 *
	 * @param regionId the regionId to set
	 */
	public void setRegionId(Long regionId) {

		this.regionId = regionId;
	}

	/**
	 * Gets the address type id.
	 *
	 * @return the addressTypeId
	 */
	public Long getAddressTypeId() {

		return addressTypeId;
	}

	/**
	 * Sets the address type id.
	 *
	 * @param addressTypeId the addressTypeId to set
	 */
	public void setAddressTypeId(Long addressTypeId) {

		this.addressTypeId = addressTypeId;
	}

	/**
	 * Gets the date moved in.
	 *
	 * @return the dateMovedIn
	 */
	public Date getDateMovedIn() {

		return dateMovedIn;
	}

	/**
	 * Sets the date moved in.
	 *
	 * @param dateMovedIn the dateMovedIn to set
	 */
	public void setDateMovedIn(Date dateMovedIn) {

		this.dateMovedIn = dateMovedIn;
	}

	/**
	 * Gets the date moved out.
	 *
	 * @return the dateMovedOut
	 */
	public Date getDateMovedOut() {

		return dateMovedOut;
	}

	/**
	 * Sets the date moved out.
	 *
	 * @param dateMovedOut the dateMovedOut to set
	 */
	public void setDateMovedOut(Date dateMovedOut) {

		this.dateMovedOut = dateMovedOut;
	}

	/**
	 * Gets the checks if is primary.
	 *
	 * @return the isPrimary
	 */
	public Boolean getIsPrimary() {

		return isPrimary;
	}

	/**
	 * Sets the checks if is primary.
	 *
	 * @param isPrimary the isPrimary to set
	 */
	public void setIsPrimary(Boolean isPrimary) {

		this.isPrimary = isPrimary;
	}

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	public String getToken() {

		return token;
	}

	/**
	 * Sets the token.
	 *
	 * @param token the token to set
	 */
	public void setToken(String token) {

		this.token = token;
	}

	/**
	 * Gets the id address abacus.
	 *
	 * @return the idAddressAbacus
	 */
	public Long getIdAddressAbacus() {

		return idAddressAbacus;
	}

	/**
	 * Sets the id address abacus.
	 *
	 * @param idAddressAbacus the idAddressAbacus to set
	 */
	public void setIdAddressAbacus(Long idAddressAbacus) {

		this.idAddressAbacus = idAddressAbacus;
	}

	/**
	 * Gets the lan.
	 *
	 * @return the lan
	 */
	public String getLan() {

		return lan;
	}

	/**
	 * Sets the lan.
	 *
	 * @param lan the new lan
	 */
	public void setLan(String lan) {

		this.lan = lan;
	}

	/**
	 * Gets the lng.
	 *
	 * @return the lng
	 */
	public String getLng() {

		return lng;
	}

	/**
	 * Sets the lng.
	 *
	 * @param lng the new lng
	 */
	public void setLng(String lng) {

		this.lng = lng;
	}

	/**
	 * Gets the supplier.
	 *
	 * @return the supplier
	 */
	public Supplier getSupplier() {

		return supplier;
	}

	/**
	 * Sets the supplier.
	 *
	 * @param supplier the new supplier
	 */
	public void setSupplier(Supplier supplier) {

		this.supplier = supplier;
	}
}
