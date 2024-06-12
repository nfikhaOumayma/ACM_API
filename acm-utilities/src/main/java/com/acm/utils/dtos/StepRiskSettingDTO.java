package com.acm.utils.dtos;

/**
 * The Class StepRiskSettingDTO.
 */
public class StepRiskSettingDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */

	private Long id;

	/** The code. */

	private WorkFlowStepDTO workFlowStep;

	/** The setting type risk. */
	private SettingTypeRiskDTO settingTypeRisk;

	/** The editable. */
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

	/**
	 * Gets the editable.
	 *
	 * @return the editable
	 */
	public Boolean getEditable() {

		return editable;
	}

	/**
	 * Gets the work flow step.
	 *
	 * @return the work flow step
	 */
	public WorkFlowStepDTO getWorkFlowStep() {

		return workFlowStep;
	}

	/**
	 * Sets the work flow step.
	 *
	 * @param workFlowStep the new work flow step
	 */
	public void setWorkFlowStep(WorkFlowStepDTO workFlowStep) {

		this.workFlowStep = workFlowStep;
	}

	/**
	 * Gets the setting type risk.
	 *
	 * @return the setting type risk
	 */
	public SettingTypeRiskDTO getSettingTypeRisk() {

		return settingTypeRisk;
	}

	/**
	 * Sets the setting type risk.
	 *
	 * @param settingTypeRisk the new setting type risk
	 */
	public void setSettingTypeRisk(SettingTypeRiskDTO settingTypeRisk) {

		this.settingTypeRisk = settingTypeRisk;
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
