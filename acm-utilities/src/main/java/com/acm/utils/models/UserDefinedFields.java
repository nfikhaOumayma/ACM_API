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
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.acm.utils.audit.AuditTrailListener;

/**
 * {@link UserDefinedFields} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_UDF_FIELD")
@EntityListeners(AuditTrailListener.class)
public class UserDefinedFields extends GenericModel
		implements Serializable, Comparable<UserDefinedFields> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7419728707618686079L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_UDF_FIELD", unique = true, nullable = false)
	private Long id;

	/** The user defined field group. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_UDF_GROUPE")
	private UserDefinedFieldGroup userDefinedFieldGroup;

	/** The id UDF field. */
	@Column(name = "ID_ABACUS_UDF_FIELD")
	private Long idUDFField;

	/** The id UDF parent field. */
	@Column(name = "ID_ACM_PARENT_UDF_FIELD")
	private Long idUDFParentField;

	/** The udf parent field value. */
	@Column(name = "ACM_PARENT_UDF_FIELD_Value")
	private String udfParentFieldValue;

	/** The field masc. */
	@Column(name = "FIELD_MASC")
	private String fieldMasc;

	/** The name. */
	@Column(name = "FIELD_NAME")
	private String name;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The mandatory. */
	@Column(name = "MANDATORY")
	private Boolean mandatory;

	/** The field type. */
	@Column(name = "FIELD_TYPE")
	private Integer fieldType;

	/** The id UDF list value. */
	@Column(name = "ID_UDF_LIST_VALUE")
	private Long idUDFListValue;

	/** The user defined fields links. */
	@OneToMany(mappedBy = "userDefinedFields")
	private Set<UserDefinedFieldsLinks> userDefinedFieldsLinks = new HashSet<>();

	/** The unique field. */
	@Column(name = "UNIQUE_FIELD")
	private Boolean uniqueField;

	/** The order. */
	@Column(name = "ORDRE")
	private Integer ordre;

	/**
	 * Instantiates a new user defined fields.
	 */
	public UserDefinedFields() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new user defined fields.
	 *
	 * @param id the id
	 */
	public UserDefinedFields(Long id) {

		this.id = id;
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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the user defined field group.
	 *
	 * @return the userDefinedFieldGroup
	 */
	public UserDefinedFieldGroup getUserDefinedFieldGroup() {

		return userDefinedFieldGroup;
	}

	/**
	 * Sets the user defined field group.
	 *
	 * @param userDefinedFieldGroup the userDefinedFieldGroup to set
	 */
	public void setUserDefinedFieldGroup(UserDefinedFieldGroup userDefinedFieldGroup) {

		this.userDefinedFieldGroup = userDefinedFieldGroup;
	}

	/**
	 * Gets the id UDF field.
	 *
	 * @return the idUDFField
	 */
	public Long getIdUDFField() {

		return idUDFField;
	}

	/**
	 * Sets the id UDF field.
	 *
	 * @param idUDFField the idUDFField to set
	 */
	public void setIdUDFField(Long idUDFField) {

		this.idUDFField = idUDFField;
	}

	/**
	 * Gets the id UDF parent field.
	 *
	 * @return the idUDFParentField
	 */
	public Long getIdUDFParentField() {

		return idUDFParentField;
	}

	/**
	 * Sets the id UDF parent field.
	 *
	 * @param idUDFParentField the idUDFParentField to set
	 */
	public void setIdUDFParentField(Long idUDFParentField) {

		this.idUDFParentField = idUDFParentField;
	}

	/**
	 * Gets the udf parent field value.
	 *
	 * @return the udfParentFieldValue
	 */
	public String getUdfParentFieldValue() {

		return udfParentFieldValue;
	}

	/**
	 * Sets the udf parent field value.
	 *
	 * @param udfParentFieldValue the udfParentFieldValue to set
	 */
	public void setUdfParentFieldValue(String udfParentFieldValue) {

		this.udfParentFieldValue = udfParentFieldValue;
	}

	/**
	 * Gets the field masc.
	 *
	 * @return the fieldMasc
	 */
	public String getFieldMasc() {

		return fieldMasc;
	}

	/**
	 * Sets the field masc.
	 *
	 * @param fieldMasc the fieldMasc to set
	 */
	public void setFieldMasc(String fieldMasc) {

		this.fieldMasc = fieldMasc;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
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
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
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
	 * @param mandatory the mandatory to set
	 */
	public void setMandatory(Boolean mandatory) {

		this.mandatory = mandatory;
	}

	/**
	 * Gets the field type.
	 *
	 * @return the fieldType
	 */
	public Integer getFieldType() {

		return fieldType;
	}

	/**
	 * Sets the field type.
	 *
	 * @param fieldType the fieldType to set
	 */
	public void setFieldType(Integer fieldType) {

		this.fieldType = fieldType;
	}

	/**
	 * Gets the id UDF list value.
	 *
	 * @return the idUDFListValue
	 */
	public Long getIdUDFListValue() {

		return idUDFListValue;
	}

	/**
	 * Sets the id UDF list value.
	 *
	 * @param idUDFListValue the idUDFListValue to set
	 */
	public void setIdUDFListValue(Long idUDFListValue) {

		this.idUDFListValue = idUDFListValue;
	}

	/**
	 * Gets the user defined fields links.
	 *
	 * @return the userDefinedFieldsLinks
	 */
	public Set<UserDefinedFieldsLinks> getUserDefinedFieldsLinks() {

		return userDefinedFieldsLinks;
	}

	/**
	 * Sets the user defined fields links.
	 *
	 * @param userDefinedFieldsLinks the userDefinedFieldsLinks to set
	 */
	public void setUserDefinedFieldsLinks(Set<UserDefinedFieldsLinks> userDefinedFieldsLinks) {

		this.userDefinedFieldsLinks = userDefinedFieldsLinks;
	}

	/**
	 * Gets the unique field.
	 *
	 * @return the unique field
	 */
	public Boolean getUniqueField() {

		return uniqueField;
	}

	/**
	 * Sets the unique field.
	 *
	 * @param uniqueField the new unique field
	 */
	public void setUniqueField(Boolean uniqueField) {

		this.uniqueField = uniqueField;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UserDefinedFields [id=" + id + ", userDefinedFieldGroup=" + userDefinedFieldGroup
				+ ", idUDFField=" + idUDFField + ", idUDFParentField=" + idUDFParentField
				+ ", udfParentFieldValue=" + udfParentFieldValue + ", fieldMasc=" + fieldMasc
				+ ", name=" + name + ", description=" + description + ", mandatory=" + mandatory
				+ ", fieldType=" + fieldType + ", idUDFListValue=" + idUDFListValue + "]";
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(UserDefinedFields userDefinedFields) {

		return this.ordre.compareTo(userDefinedFields.getOrdre());
	}

}
