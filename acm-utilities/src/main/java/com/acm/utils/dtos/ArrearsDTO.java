/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link ArrearsDTO} class.
 * 
 * @author Salmen Fatnassi
 * @since 1.1.3
 */
public class ArrearsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5591847609309150793L;

	/** The customer id. */
	private Long customerIdExtern;

	/** The arrear day. */
	private Long arrearDay;

	/** The arrear schedule. */
	private Long arrearSchedule;

	/**
	 * Instantiates a new arrears DTO.
	 */
	public ArrearsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new arrears DTO.
	 *
	 * @param customerIdExtern the customer id extern
	 * @param arrearDay the arrear day
	 * @param arrearSchedule the arrear schedule
	 */
	public ArrearsDTO(Long customerIdExtern, Long arrearDay, Long arrearSchedule) {

		this.customerIdExtern = customerIdExtern;
		this.arrearDay = arrearDay;
		this.arrearSchedule = arrearSchedule;
	}

	/**
	 * Instantiates a new arrears DTO.
	 *
	 * @param customerIdExtern the customer id extern
	 */
	public ArrearsDTO(Long customerIdExtern) {

		this.customerIdExtern = customerIdExtern;
	}

	/**
	 * Gets the customer id extern.
	 *
	 * @return the customer id extern
	 */
	public Long getCustomerIdExtern() {

		return customerIdExtern;
	}

	/**
	 * Sets the customer id extern.
	 *
	 * @param customerIdExtern the new customer id extern
	 */
	public void setCustomerIdExtern(Long customerIdExtern) {

		this.customerIdExtern = customerIdExtern;
	}

	/**
	 * Gets the arrear day.
	 *
	 * @return the arrear day
	 */
	public Long getArrearDay() {

		return arrearDay;
	}

	/**
	 * Sets the arrear day.
	 *
	 * @param arrearDay the new arrear day
	 */
	public void setArrearDay(Long arrearDay) {

		this.arrearDay = arrearDay;
	}

	/**
	 * Gets the arrear schedule.
	 *
	 * @return the arrear schedule
	 */
	public Long getArrearSchedule() {

		return arrearSchedule;
	}

	/**
	 * Sets the arrear schedule.
	 *
	 * @param arrearSchedule the new arrear schedule
	 */
	public void setArrearSchedule(Long arrearSchedule) {

		this.arrearSchedule = arrearSchedule;
	}

}
