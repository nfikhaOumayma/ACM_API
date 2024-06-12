/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link CustomerAbacusAPIModel3} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class CustomerAbacusAPIModelCustomerAddress implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7690794004366824924L;

	/** The customer address ID. */
	private int customerAddressID;

	/** The customer ID. */
	private int customerID;

	/** The address. */
	private CustomerAbacusAPIModelAddress address;

	/** The address type ID. */
	private int addressTypeID;

	/** The date moved in. */
	private Date dateMovedIn;

	/** The date moved out. */
	private Date dateMovedOut;

	/** The is primary. */
	private boolean isPrimary;

	/**
	 * Gets the customer address ID.
	 *
	 * @return the customerAddressID
	 */
	public int getCustomerAddressID() {

		return customerAddressID;
	}

	/**
	 * Sets the customer address ID.
	 *
	 * @param customerAddressID the customerAddressID to set
	 */
	public void setCustomerAddressID(int customerAddressID) {

		this.customerAddressID = customerAddressID;
	}

	/**
	 * Gets the customer ID.
	 *
	 * @return the customerID
	 */
	public int getCustomerID() {

		return customerID;
	}

	/**
	 * Sets the customer ID.
	 *
	 * @param customerID the customerID to set
	 */
	public void setCustomerID(int customerID) {

		this.customerID = customerID;
	}

	/**
	 * Gets the address.
	 *
	 * @return the address
	 */
	public CustomerAbacusAPIModelAddress getAddress() {

		return address;
	}

	/**
	 * Sets the address.
	 *
	 * @param address the address to set
	 */
	public void setAddress(CustomerAbacusAPIModelAddress address) {

		this.address = address;
	}

	/**
	 * Gets the address type ID.
	 *
	 * @return the addressTypeID
	 */
	public int getAddressTypeID() {

		return addressTypeID;
	}

	/**
	 * Sets the address type ID.
	 *
	 * @param addressTypeID the addressTypeID to set
	 */
	public void setAddressTypeID(int addressTypeID) {

		this.addressTypeID = addressTypeID;
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
	 * Checks if is primary.
	 *
	 * @return the isPrimary
	 */
	public boolean isPrimary() {

		return isPrimary;
	}

	/**
	 * Sets the primary.
	 *
	 * @param isPrimary the isPrimary to set
	 */
	public void setPrimary(boolean isPrimary) {

		this.isPrimary = isPrimary;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelCustomerAddress [customerAddressID=" + customerAddressID
				+ ", customerID=" + customerID + ", address=" + address + ", addressTypeID="
				+ addressTypeID + ", dateMovedIn=" + dateMovedIn + ", dateMovedOut=" + dateMovedOut
				+ ", isPrimary=" + isPrimary + "]";
	}

}
