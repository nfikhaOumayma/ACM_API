/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link AcmIhmField} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Entity
@Table(name = "ACM_IHM_FIELD")
@NamedQuery(name = "AcmIhmField.findAll", query = "SELECT l FROM AcmIhmField l")
public class AcmIhmField extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8414792404488831423L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The code field. */
	@Column(name = "CODE_FIELD", length = 256)
	private String codeField;

	/** The form control name. */
	@Column(name = "FORM_CONTROL_NAME", length = 256)
	private String formControlName;

	/** The sub code field. */
	@Column(name = "SUB_CODE_FIELD", length = 256)
	private String subCodeField;

	/** The type field. */
	@Column(name = "TYPE_FIELD", length = 256)
	private String typeField;

	/** The default value. */
	@Column(name = "DEFAULT_VALUE", length = 256)
	private String defaultValue;

	/** The titre. */
	@Column(name = "TITLE", length = 256)
	private String titre;

	/** The description. */
	@Column(name = "DESCRIPTION", length = 256)
	private String description;

	/** The placeholder. */
	@Column(name = "PLACEHOLDER", length = 256)
	private String placeholder;

	/** The min. */
	@Column(name = "MIN", length = 256)
	private Integer min;

	/** The max. */
	@Column(name = "MAX", length = 256)
	private Integer max;

	/** The step. */
	@Column(name = "STEP", length = 256)
	private Integer step;

	/** The single select. */
	@Column(name = "SINGLE_SELECT", length = 256)
	private Boolean singleSelect;

	/** The type option values. */
	@Column(name = "TYPE_OPTION_VALUE", length = 256)
	private String typeOptionValues;

	/** The single select. */
	@Column(name = "ORDRE", length = 256)
	private Integer ordre;

	/** The acm ihm form. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_ACM_IHM_FORM")
	private AcmIhmForm acmIhmForm;

	/** The validators. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_IHM_VALIDATOR_FIELD",
			joinColumns = {@JoinColumn(name = "ID_ACM_IHM_FIELD")},
			inverseJoinColumns = {@JoinColumn(name = "ID_ACM_IHM_VALIDATOR")})
	private Set<AcmIhmValidator> validators = new HashSet<>();

	/** The acm ihm field groupes. */
	@OneToMany(mappedBy = "acmIhmField")
	private Set<AcmIhmFieldGroupe> acmIhmFieldGroupes = new HashSet<>();

	/**
	 * Instantiates a new acm ihm field.
	 */
	public AcmIhmField() {

		// EMPTY
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
	public Set<AcmIhmValidator> getValidators() {

		return validators;
	}

	/**
	 * Sets the validators.
	 *
	 * @param validators the new validators
	 */
	public void setValidators(Set<AcmIhmValidator> validators) {

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
	 * Gets the acm ihm form.
	 *
	 * @return the acm ihm form
	 */
	public AcmIhmForm getAcmIhmForm() {

		return acmIhmForm;
	}

	/**
	 * Sets the acm ihm form.
	 *
	 * @param acmIhmForm the new acm ihm form
	 */
	public void setAcmIhmForm(AcmIhmForm acmIhmForm) {

		this.acmIhmForm = acmIhmForm;
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
	 * Gets the min.
	 *
	 * @return the min
	 */
	public Integer getMin() {

		return min;
	}

	/**
	 * Sets the min.
	 *
	 * @param min the new min
	 */
	public void setMin(Integer min) {

		this.min = min;
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
	 * Gets the step.
	 *
	 * @return the step
	 */
	public Integer getStep() {

		return step;
	}

	/**
	 * Sets the step.
	 *
	 * @param step the new step
	 */
	public void setStep(Integer step) {

		this.step = step;
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
	 * Gets the type option values.
	 *
	 * @return the type option values
	 */
	public String getTypeOptionValues() {

		return typeOptionValues;
	}

	/**
	 * Sets the type option values.
	 *
	 * @param typeOptionValues the new type option values
	 */
	public void setTypeOptionValues(String typeOptionValues) {

		this.typeOptionValues = typeOptionValues;
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
	 * Gets the acm ihm field groupes.
	 *
	 * @return the acm ihm field groupes
	 */
	public Set<AcmIhmFieldGroupe> getAcmIhmFieldGroupes() {

		return acmIhmFieldGroupes;
	}

	/**
	 * Sets the acm ihm field groupes.
	 *
	 * @param acmIhmFieldGroupes the new acm ihm field groupes
	 */
	public void setAcmIhmFieldGroupes(Set<AcmIhmFieldGroupe> acmIhmFieldGroupes) {

		this.acmIhmFieldGroupes = acmIhmFieldGroupes;
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

}
