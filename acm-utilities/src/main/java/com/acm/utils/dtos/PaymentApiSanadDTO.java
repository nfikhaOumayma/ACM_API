/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class PaymentApiSanadDTO.
 */
public class PaymentApiSanadDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6008146527998222876L;

	/** The action. */
	public String action;

	/** The trackid. */
	public String trackid;

	/** The terminal id. */
	public String terminalId;

	/** The password. */
	public String password;

	/** The amount. */
	public String amount;

	/** The currency. */
	public String currency;

	/** The zip code. */
	public String zipCode;

	/** The country. */
	public String country;

	/** The city. */
	public String city;

	/** The state. */
	public String state;

	/** The address. */
	public String address;

	/** The customer email. */
	public String customerEmail;

	/** The contact number. */
	public String contactNumber;

	/** The meta data. */
	public String metaData;

	/** The request hash. */
	public String requestHash;

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
	 * Gets the trackid.
	 *
	 * @return the trackid
	 */
	public String getTrackid() {

		return trackid;
	}

	/**
	 * Sets the trackid.
	 *
	 * @param trackid the new trackid
	 */
	public void setTrackid(String trackid) {

		this.trackid = trackid;
	}

	/**
	 * Gets the terminal id.
	 *
	 * @return the terminal id
	 */
	public String getTerminalId() {

		return terminalId;
	}

	/**
	 * Sets the terminal id.
	 *
	 * @param terminalId the new terminal id
	 */
	public void setTerminalId(String terminalId) {

		this.terminalId = terminalId;
	}

	/**
	 * Gets the password.
	 *
	 * @return the password
	 */
	public String getPassword() {

		return password;
	}

	/**
	 * Sets the password.
	 *
	 * @param password the new password
	 */
	public void setPassword(String password) {

		this.password = password;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public String getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(String amount) {

		this.amount = amount;
	}

	/**
	 * Gets the currency.
	 *
	 * @return the currency
	 */
	public String getCurrency() {

		return currency;
	}

	/**
	 * Sets the currency.
	 *
	 * @param currency the new currency
	 */
	public void setCurrency(String currency) {

		this.currency = currency;
	}

	/**
	 * Gets the zip code.
	 *
	 * @return the zip code
	 */
	public String getZipCode() {

		return zipCode;
	}

	/**
	 * Sets the zip code.
	 *
	 * @param zipCode the new zip code
	 */
	public void setZipCode(String zipCode) {

		this.zipCode = zipCode;
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
	 * @param country the new country
	 */
	public void setCountry(String country) {

		this.country = country;
	}

	/**
	 * Gets the city.
	 *
	 * @return the city
	 */
	public String getCity() {

		return city;
	}

	/**
	 * Sets the city.
	 *
	 * @param city the new city
	 */
	public void setCity(String city) {

		this.city = city;
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
	 * @param state the new state
	 */
	public void setState(String state) {

		this.state = state;
	}

	/**
	 * Gets the address.
	 *
	 * @return the address
	 */
	public String getAddress() {

		return address;
	}

	/**
	 * Sets the address.
	 *
	 * @param address the new address
	 */
	public void setAddress(String address) {

		this.address = address;
	}

	/**
	 * Gets the customer email.
	 *
	 * @return the customer email
	 */
	public String getCustomerEmail() {

		return customerEmail;
	}

	/**
	 * Sets the customer email.
	 *
	 * @param customerEmail the new customer email
	 */
	public void setCustomerEmail(String customerEmail) {

		this.customerEmail = customerEmail;
	}

	/**
	 * Gets the contact number.
	 *
	 * @return the contact number
	 */
	public String getContactNumber() {

		return contactNumber;
	}

	/**
	 * Sets the contact number.
	 *
	 * @param contactNumber the new contact number
	 */
	public void setContactNumber(String contactNumber) {

		this.contactNumber = contactNumber;
	}

	/**
	 * Gets the meta data.
	 *
	 * @return the meta data
	 */
	public String getMetaData() {

		return metaData;
	}

	/**
	 * Sets the meta data.
	 *
	 * @param metaData the new meta data
	 */
	public void setMetaData(String metaData) {

		this.metaData = metaData;
	}

	/**
	 * Gets the request hash.
	 *
	 * @return the request hash
	 */
	public String getRequestHash() {

		return requestHash;
	}

	/**
	 * Sets the request hash.
	 *
	 * @param requestHash the new request hash
	 */
	public void setRequestHash(String requestHash) {

		this.requestHash = requestHash;
	}

	/**
	 * Gets the serialversionuid.
	 *
	 * @return the serialversionuid
	 */
	public static long getSerialversionuid() {

		return serialVersionUID;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "PaymentApiSanadDTO [action=" + action + ", trackid=" + trackid + ", terminalId="
				+ terminalId + ", password=" + password + ", amount=" + amount + ", currency="
				+ currency + ", zipCode=" + zipCode + ", country=" + country + ", city=" + city
				+ ", state=" + state + ", address=" + address + ", customerEmail=" + customerEmail
				+ ", contactNumber=" + contactNumber + ", metaData=" + metaData + ", requestHash="
				+ requestHash + "]";
	}

}
