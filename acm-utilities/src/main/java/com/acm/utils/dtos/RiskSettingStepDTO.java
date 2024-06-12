package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class RiskSettingStepDTO.
 */
public class RiskSettingStepDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The id risk setting. */
	private Long idRiskSetting;

	/** The is editable. */
	private Boolean isEditable;

	/** The label risk. */
	private String labelRisk;

	/**
	 * Gets the id risk setting.
	 *
	 * @return the id risk setting
	 */
	public Long getIdRiskSetting() {

		return idRiskSetting;
	}

	/**
	 * Sets the id risk setting.
	 *
	 * @param idRiskSetting the new id risk setting
	 */
	public void setIdRiskSetting(Long idRiskSetting) {

		this.idRiskSetting = idRiskSetting;
	}

	/**
	 * Gets the checks if is editable.
	 *
	 * @return the checks if is editable
	 */
	public Boolean getIsEditable() {

		return isEditable;
	}

	/**
	 * Sets the checks if is editable.
	 *
	 * @param isEditable the new checks if is editable
	 */
	public void setIsEditable(Boolean isEditable) {

		this.isEditable = isEditable;
	}

	/**
	 * Gets the label risk.
	 *
	 * @return the label risk
	 */
	public String getLabelRisk() {

		return labelRisk;
	}

	/**
	 * Sets the label risk.
	 *
	 * @param labelRisk the new label risk
	 */
	public void setLabelRisk(String labelRisk) {

		this.labelRisk = labelRisk;
	}

}
