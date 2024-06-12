/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link ItemRiskSetting} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_STEP_RISQUE_SETTING")
public class StepRiskSetting implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_STEP_RISQUE_SETTING", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_ACM_WORKFLOW_STEP")
	private WorkFlowStep workFlowStep;

	/** The setting type risk. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_TYPE_RISQUE")
	private SettingTypeRisk settingTypeRisk;

	/** The editable. */
	@Column(name = "EDITABLE")
	private Boolean editable;

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
	 * Gets the work flow step.
	 *
	 * @return the work flow step
	 */
	public WorkFlowStep getWorkFlowStep() {

		return workFlowStep;
	}

	/**
	 * Sets the work flow step.
	 *
	 * @param workFlowStep the new work flow step
	 */
	public void setWorkFlowStep(WorkFlowStep workFlowStep) {

		this.workFlowStep = workFlowStep;
	}

	/**
	 * Gets the setting type risk.
	 *
	 * @return the setting type risk
	 */
	public SettingTypeRisk getSettingTypeRisk() {

		return settingTypeRisk;
	}

	/**
	 * Sets the setting type risk.
	 *
	 * @param settingTypeRisk the new setting type risk
	 */
	public void setSettingTypeRisk(SettingTypeRisk settingTypeRisk) {

		this.settingTypeRisk = settingTypeRisk;
	}

	/**
	 * Gets the editable.
	 *
	 * @return the editable
	 */
	public Boolean getEditable() {

		return editable;
	}

	/**
	 * Sets the editable.
	 *
	 * @param editable the new editable
	 */
	public void setEditable(Boolean editable) {

		this.editable = editable;
	}

}
