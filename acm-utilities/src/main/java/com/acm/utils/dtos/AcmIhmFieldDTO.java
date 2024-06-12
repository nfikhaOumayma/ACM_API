/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

import org.dozer.Mapping;

/**
 * {@link AcmIhmFieldDTO} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public class AcmIhmFieldDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3868475557529653180L;

	/** The id. */
	private Long id;

	/** The code field. */
	private String codeField;

	/** The form control name. */
	private String formControlName;

	/** The sub code field. */
	private String subCodeField;

	/** The type field. */
	private String typeField;

	/** The default value. */
	private String defaultValue;

	/** The max. */
	private Integer max;

	/** The habilitation. */
	private String habilitation;

	/** The titre. */
	private String titre;

	/** The description. */
	private String description;

	/** The placeholder. */
	private String placeholder;

	/** The single select. */
	private Boolean singleSelect;

	/** The ordre. */
	private Integer ordre;

	/** The code page. */
	private String codeForm;

	/** The enabled. */
	private Boolean enabled;

	/** The acm ihm form. */
	private AcmIhmFormDTO acmIhmFormDTO;
	/** The code user group. */
	private String codeUserGroup;

	/** The acm ihm validator DT os. */
	@Mapping("validators")
	private List<AcmIhmValidatorDTO> validators;

	/**
	 * Instantiates a new acm ihm field DTO.
	 */
	public AcmIhmFieldDTO() {

		// EMPTY
	}

	/**
	 * Instantiates a new acm ihm field DTO.
	 *
	 * @param codeForm the code form
	 */
	public AcmIhmFieldDTO(String codeForm) {

		this.codeForm = codeForm;
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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the code field.
	 *
	 * @return the code field
	 */
	public String getCodeField() {

		return codeField;
	}

	/**
	 * Sets the code field.
	 *
	 * @param codeField the new code field
	 */
	public void setCodeField(String codeField) {

		this.codeField = codeField;
	}

	/**
	 * Gets the default value.
	 *
	 * @return the default value
	 */
	public String getDefaultValue() {

		return defaultValue;
	}

	/**
	 * Sets the default value.
	 *
	 * @param defaultValue the new default value
	 */
	public void setDefaultValue(String defaultValue) {

		this.defaultValue = defaultValue;
	}

	/**
	 * Gets the validators.
	 *
	 * @return the validators
	 */
	public List<AcmIhmValidatorDTO> getValidators() {

		return validators;
	}

	/**
	 * Sets the validators.
	 *
	 * @param validators the validators to set
	 */
	public void setValidators(List<AcmIhmValidatorDTO> validators) {

		this.validators = validators;
	}

	/**
	 * Gets the type field.
	 *
	 * @return the type field
	 */
	public String getTypeField() {

		return typeField;
	}

	/**
	 * Sets the type field.
	 *
	 * @param typeField the new type field
	 */
	public void setTypeField(String typeField) {

		this.typeField = typeField;
	}

	/**
	 * Gets the habilitation.
	 *
	 * @return the habilitation
	 */
	public String getHabilitation() {

		return habilitation;
	}

	/**
	 * Sets the habilitation.
	 *
	 * @param habilitation the new habilitation
	 */
	public void setHabilitation(String habilitation) {

		this.habilitation = habilitation;
	}

	/**
	 * Gets the titre.
	 *
	 * @return the titre
	 */
	public String getTitre() {

		return titre;
	}

	/**
	 * Sets the titre.
	 *
	 * @param titre the new titre
	 */
	public void setTitre(String titre) {

		this.titre = titre;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the placeholder.
	 *
	 * @return the placeholder
	 */
	public String getPlaceholder() {

		return placeholder;
	}

	/**
	 * Sets the placeholder.
	 *
	 * @param placeholder the new placeholder
	 */
	public void setPlaceholder(String placeholder) {

		this.placeholder = placeholder;
	}

	/**
	 * Gets the single select.
	 *
	 * @return the single select
	 */
	public Boolean getSingleSelect() {

		return singleSelect;
	}

	/**
	 * Sets the single select.
	 *
	 * @param singleSelect the new single select
	 */
	public void setSingleSelect(Boolean singleSelect) {

		this.singleSelect = singleSelect;
	}

	/**
	 * Gets the ordre.
	 *
	 * @return the ordre
	 */
	public Integer getOrdre() {

		return ordre;
	}

	/**
	 * Sets the ordre.
	 *
	 * @param ordre the new ordre
	 */
	public void setOrdre(Integer ordre) {

		this.ordre = ordre;
	}

	/**
	 * Gets the code form.
	 *
	 * @return the code form
	 */
	public String getCodeForm() {

		return codeForm;
	}

	/**
	 * Sets the code form.
	 *
	 * @param codeForm the new code form
	 */
	public void setCodeForm(String codeForm) {

		this.codeForm = codeForm;
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the form control name.
	 *
	 * @return the form control name
	 */
	public String getFormControlName() {

		return formControlName;
	}

	/**
	 * Sets the form control name.
	 *
	 * @param formControlName the new form control name
	 */
	public void setFormControlName(String formControlName) {

		this.formControlName = formControlName;
	}

	/**
	 * Gets the max.
	 *
	 * @return the max
	 */
	public Integer getMax() {

		return max;
	}

	/**
	 * Sets the max.
	 *
	 * @param max the new max
	 */
	public void setMax(Integer max) {

		this.max = max;
	}

	/**
	 * Gets the acm ihm form DTO.
	 *
	 * @return the acm ihm form DTO
	 */
	public AcmIhmFormDTO getAcmIhmFormDTO() {

		return acmIhmFormDTO;
	}

	/**
	 * Sets the acm ihm form DTO.
	 *
	 * @param acmIhmFormDTO the new acm ihm form DTO
	 */
	public void setAcmIhmFormDTO(AcmIhmFormDTO acmIhmFormDTO) {

		this.acmIhmFormDTO = acmIhmFormDTO;
	}

	/**
	 * Gets the sub code field.
	 *
	 * @return the sub code field
	 */
	public String getSubCodeField() {

		return subCodeField;
	}

	/**
	 * Sets the sub code field.
	 *
	 * @param subCodeField the new sub code field
	 */
	public void setSubCodeField(String subCodeField) {

		this.subCodeField = subCodeField;
	}

	/**
	 * Gets the code user group.
	 *
	 * @return the code user group
	 */
	public String getCodeUserGroup() {

		return codeUserGroup;
	}

	/**
	 * Sets the code user group.
	 *
	 * @param codeUserGroup the new code user group
	 */
	public void setCodeUserGroup(String codeUserGroup) {

		this.codeUserGroup = codeUserGroup;
	}

}
