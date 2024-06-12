/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

// TODO: Auto-generated Javadoc
/**
 * {@link OnboardCustomerResponse } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class OnboardCustomerResponse {

	/** The risk calculation id. */
	private Long riskCalculationId;

	/** The instruction. */
	private Instruction instruction;

	/** The error message. */
	private String errorMessage;

	/**
	 * Gets the risk calculation id.
	 *
	 * @return the riskCalculationId
	 */
	public Long getRiskCalculationId() {

		return riskCalculationId;
	}

	/**
	 * Sets the risk calculation id.
	 *
	 * @param riskCalculationId the riskCalculationId to set
	 */
	public void setRiskCalculationId(Long riskCalculationId) {

		this.riskCalculationId = riskCalculationId;
	}

	/**
	 * Gets the instruction.
	 *
	 * @return the instruction
	 */
	public Instruction getInstruction() {

		return instruction;
	}

	/**
	 * Sets the instruction.
	 *
	 * @param instruction the new instruction
	 */
	public void setInstruction(Instruction instruction) {

		this.instruction = instruction;
	}

	/**
	 * Gets the error message.
	 *
	 * @return the errorMessage
	 */
	public String getErrorMessage() {

		return errorMessage;
	}

	/**
	 * Sets the error message.
	 *
	 * @param errorMessage the errorMessage to set
	 */
	public void setErrorMessage(String errorMessage) {

		this.errorMessage = errorMessage;
	}

}
