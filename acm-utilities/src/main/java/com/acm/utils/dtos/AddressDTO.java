/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonBackReference;

// TODO: Auto-generated Javadoc
/**
 * The {@link Address} class.
 * 
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class AddressDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6718157823038459604L;

	/** The id. */
	private Long id;

	/** The id address abacus. */
	private Long idAddressAbacus;

	/** The customer id. */
	private Long customerId;

	/** The address 1. */
	private String address1;

	/** The address 2. */
	private String address2;

	/** The address 3. */
	private String address3;

	/** The town city. */
	private String townCity;

	/** The county. */
	private String county;

	/** The state. */
	private String state;

	/** The postal code. */
	private String postalCode;

	/** The country. */
	private String country;

	/** The region. */
	private String region;

	/** The address 1 id. */
	private Long address1Id;

	/** The address 2 id. */
	private Long address2Id;

	/** The address 3 id. */
	private Long address3Id;

	/** The town city id. */
	private Long townCityId;

	/** The county id. */
	private Long countyId;

	/** The state id. */
	private Long stateId;

	/** The postal code id. */
	private Long postalCodeId;

	/** The country id. */
	private Long countryId;

	/** The region id. */
	private Long regionId;

	/** The address type id. */
	private Long addressTypeId;

	/** The date moved in. */
	private Date dateMovedIn;

	/** The date moved out. */
	private Date dateMovedOut;

	/** The is primary. */
	private Boolean isPrimary;

	/** The token. */
	private String token;

	/** The latitude. */
	private String lan;

	/** The longitude. */
	private String lng;

	/** The action. */
	private String action;

	/** The reason update. */
	private String reasonUpdate;
	
	/** The supplier id. */
	@JsonBackReference
	private SupplierDTO supplier;

	/**
	 * Instantiates a new address DTO.
	 */
	public AddressDTO() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new address DTO.
	 *
	 * @param customerId the customer id
	 * @param address1 the address 1
	 * @param address2 the address 2
	 * @param address3 the address 3
	 * @param townCity the town city
	 * @param county the county
	 * @param state the state
	 * @param postalCode the postal code
	 * @param country the country
	 * @param region the region
	 * @param address1Id the address 1 id
	 * @param address2Id the address 2 id
	 * @param address3Id the address 3 id
	 * @param townCityId the town city id
	 * @param countyId the county id
	 * @param stateId the state id
	 * @param postalCodeId the postal code id
	 * @param countryId the country id
	 * @param regionId the region id
	 * @param addressTypeId the address type id
	 * @param dateMovedIn the date moved in
	 * @param dateMovedOut the date moved out
	 * @param isPrimary the is primary
	 * @param idAddressAbacus the id address abacus
	 */
	public AddressDTO(Long customerId, String address1, String address2, String address3,
			String townCity, String county, String state, String postalCode, String country,
			String region, Long address1Id, Long address2Id, Long address3Id, Long townCityId,
			Long countyId, Long stateId, Long postalCodeId, Long countryId, Long regionId,
			Long addressTypeId, Date dateMovedIn, Date dateMovedOut, Boolean isPrimary,
			Long idAddressAbacus) {

		this.customerId = customerId;
		this.address1 = address1;
		this.address2 = address2;
		this.address3 = address3;
		this.townCity = townCity;
		this.county = county;
		this.state = state;
		this.postalCode = postalCode;
		this.country = country;
		this.region = region;
		this.address1Id = address1Id;
		this.address2Id = address2Id;
		this.address3Id = address3Id;
		this.townCityId = townCityId;
		this.countyId = countyId;
		this.stateId = stateId;
		this.postalCodeId = postalCodeId;
		this.countryId = countryId;
		this.regionId = regionId;
		this.addressTypeId = addressTypeId;
		this.dateMovedIn = dateMovedIn;
		this.dateMovedOut = dateMovedOut;
		this.isPrimary = isPrimary;
		this.idAddressAbacus = idAddressAbacus;
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
	 * Gets the action.
	 *
	 * @return the action
	 */
	public String getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the new action
	 */
	public void setAction(String action) {

		this.action = action;
	}

	/**
	 * Gets the reason update.
	 *
	 * @return the reasonUpdate
	 */
	public String getReasonUpdate() {

		return reasonUpdate;
	}

	/**
	 * Sets the reason update.
	 *
	 * @param reasonUpdate the reasonUpdate to set
	 */
	public void setReasonUpdate(String reasonUpdate) {

		this.reasonUpdate = reasonUpdate;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AddressDTO [id=" + id + ", idAddressAbacus=" + idAddressAbacus + ", customerId="
				+ customerId + ", address1=" + address1 + ", address2=" + address2 + ", address3="
				+ address3 + ", townCity=" + townCity + ", county=" + county + ", state=" + state
				+ ", postalCode=" + postalCode + ", country=" + country + ", region=" + region
				+ ", address1Id=" + address1Id + ", address2Id=" + address2Id + ", address3Id="
				+ address3Id + ", townCityId=" + townCityId + ", countyId=" + countyId
				+ ", stateId=" + stateId + ", postalCodeId=" + postalCodeId + ", countryId="
				+ countryId + ", regionId=" + regionId + ", addressTypeId=" + addressTypeId
				+ ", dateMovedIn=" + dateMovedIn + ", dateMovedOut=" + dateMovedOut + ", isPrimary="
				+ isPrimary + "]";
	}

	/**
	 * Formatted ADDRESS.
	 * 
	 * @author HaythemBenizid
	 * @return the string
	 */
	public String formattedAddress() {

		return (address1 != null ? address1 : "") + (address2 != null ? address2 : "")
				+ (address3 != null ? address3 : "") + (townCity != null ? " " + townCity : "")
				+ (county != null ? " " + county : "") + (state != null ? " " + state : "")
				+ (postalCode != null ? " " + postalCode : "");
	}

	/**
	 * Gets the supplier.
	 *
	 * @return the supplier
	 */
	public SupplierDTO getSupplier() {

		return supplier;
	}

	/**
	 * Sets the supplier.
	 *
	 * @param supplier the new supplier
	 */
	public void setSupplier(SupplierDTO supplier) {

		this.supplier = supplier;
	}



}
