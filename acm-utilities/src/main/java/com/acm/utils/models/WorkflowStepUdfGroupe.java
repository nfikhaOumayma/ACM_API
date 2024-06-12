/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The Class WorkflowStepUdfGroupe.
 */
@Entity
@Table(name = "ACM_UDF_STEP_WORKFLOW")
public class WorkflowStepUdfGroupe implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7127718119619815549L;

	/** The id work flow step udf group. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long idWorkFlowStepUdfGroup;

	/** The user defined fields. */
	@Column(name = "ID_UDF_FIELD")
	private Long idUserDefinedFields;

	/** The user defined field group. */
	@Column(name = "ID_UDF_GROUP")
	private Long idUserDefinedFieldGroup;

	/** The workflow step. */
	@Column(name = "ID_ACM_WORKFLOW_STEP")
	private Long idWorkflowStep;
	/** The mandatory. */
	@Column(name = "MANDATORY")
	private Boolean mandatory;

	/** The collection step. */
	@Column(name = "ID_ACM_COLLECTION_STEP")
	private Long idCollectionStep;

	/**
	 * Gets the collection step.
	 *
	 * @return the collection step
	 */

	/**
	 * Gets the id work flow step udf group.
	 *
	 * @return the id work flow step udf group
	 */
	public Long getIdWorkFlowStepUdfGroup() {

		return idWorkFlowStepUdfGroup;
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
	 * Sets the id work flow step udf group.
	 *
	 * @param idWorkFlowStepUdfGroup the new id work flow step udf group
	 */
	public void setIdWorkFlowStepUdfGroup(Long idWorkFlowStepUdfGroup) {

		this.idWorkFlowStepUdfGroup = idWorkFlowStepUdfGroup;
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
	 * @param idWserDefinedFieldGroup the new id user defined field group
	 */
	public void setIdUserDefinedFieldGroup(Long idWserDefinedFieldGroup) {

		this.idUserDefinedFieldGroup = idWserDefinedFieldGroup;
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

	/**
	 * Instantiates a new workflow step udf groupe.
	 *
	 * @param idWorkFlowStepUdfGroup the id work flow step udf group
	 * @param idUserDefinedFields the id user defined fields
	 * @param idUserDefinedFieldGroup the id user defined field group
	 * @param idWorkflowStep the id workflow step
	 * @param mandatory the mandatory
	 * @param idCollectionStep the id collection step
	 */
	public WorkflowStepUdfGroupe(Long idWorkFlowStepUdfGroup, Long idUserDefinedFields,
			Long idUserDefinedFieldGroup, Long idWorkflowStep, Boolean mandatory,
			Long idCollectionStep) {

		super();
		this.idWorkFlowStepUdfGroup = idWorkFlowStepUdfGroup;
		this.idUserDefinedFields = idUserDefinedFields;
		this.idUserDefinedFieldGroup = idUserDefinedFieldGroup;
		this.idWorkflowStep = idWorkflowStep;
		this.mandatory = mandatory;
		this.idCollectionStep = idCollectionStep;
	}

	/**
	 * Instantiates a new workflow step udf groupe.
	 */
	public WorkflowStepUdfGroupe() {

		super();
	}

	/**
	 * Gets the user defined fields.
	 *
	 * @return the user defined fields
	 */

}
