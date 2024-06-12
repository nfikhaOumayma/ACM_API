/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class WorkflowStepUdfGroupeDTO.
 */
public class WorkflowStepUdfGroupeDTO implements Serializable {
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7127718119619815549L;

	/** The id work flow step udf group. */
	private Long idWorkFlowStepUdfGroup;

	/** The user defined field group. */
	private Long idUserDefinedFieldGroup;

	/** The workflow step. */
	private Long idWorkflowStep;

	/** The user defined fields. */
	private Long idUserDefinedFields;

	/** The collection step. */
	private Long idCollectionStep;
	/** The mandatory. */
	private Boolean mandatory;

	/**
	 * Gets the id work flow step udf group.
	 *
	 * @return the id work flow step udf group
	 */
	public Long getIdWorkFlowStepUdfGroup() {

		return idWorkFlowStepUdfGroup;
	}

	/**
	 * Sets the id work flow step udf group.
	 *
	 * @param idWorkFlowStepUdfGroup the new id work flow step udf group
	 */
	public void setIdWorkFlowStepUdfGroup(Long idWorkFlowStepUdfGroup) {

		this.idWorkFlowStepUdfGroup = idWorkFlowStepUdfGroup;
	}

	/**
	 * Gets the id user defined field group.
	 *
	 * @return the id user defined field group
	 */
	public Long getIdUserDefinedFieldGroup() {

		return idUserDefinedFieldGroup;
	}

	/**
	 * Sets the id user defined field group.
	 *
	 * @param idUserDefinedFieldGroup the new id user defined field group
	 */
	public void setIdUserDefinedFieldGroup(Long idUserDefinedFieldGroup) {

		this.idUserDefinedFieldGroup = idUserDefinedFieldGroup;
	}

	/**
	 * Gets the id workflow step.
	 *
	 * @return the id workflow step
	 */
	public Long getIdWorkflowStep() {

		return idWorkflowStep;
	}

	/**
	 * Sets the id workflow step.
	 *
	 * @param idWorkflowStep the new id workflow step
	 */
	public void setIdWorkflowStep(Long idWorkflowStep) {

		this.idWorkflowStep = idWorkflowStep;
	}

	/**
	 * Gets the id user defined fields.
	 *
	 * @return the id user defined fields
	 */
	public Long getIdUserDefinedFields() {

		return idUserDefinedFields;
	}

	/**
	 * Sets the id user defined fields.
	 *
	 * @param idUserDefinedFields the new id user defined fields
	 */
	public void setIdUserDefinedFields(Long idUserDefinedFields) {

		this.idUserDefinedFields = idUserDefinedFields;
	}

	/**
	 * Gets the id collection step.
	 *
	 * @return the id collection step
	 */
	public Long getIdCollectionStep() {

		return idCollectionStep;
	}

	/**
	 * Sets the id collection step.
	 *
	 * @param idCollectionStep the new id collection step
	 */
	public void setIdCollectionStep(Long idCollectionStep) {

		this.idCollectionStep = idCollectionStep;
	}

	/**
	 * Gets the mandatory.
	 *
	 * @return the mandatory
	 */
	public Boolean getMandatory() {

		return mandatory;
	}

	/**
	 * Sets the mandatory.
	 *
	 * @param mandatory the new mandatory
	 */
	public void setMandatory(Boolean mandatory) {

		this.mandatory = mandatory;
	}

}
