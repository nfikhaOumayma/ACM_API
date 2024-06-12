/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.dozer.Mapping;

import com.acm.utils.enums.SettingUDFFieldsType;
import com.acm.utils.models.UserDefinedFields;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link UserDefinedFields} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class UserDefinedFieldsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2431210191699768964L;

	/** The id. */
	private Long id;

	/** The user defined field group. */
	@Mapping("userDefinedFieldGroup")
	private UserDefinedFieldGroupDTO userDefinedFieldGroupDTO;

	/** The id UDF field. */
	private Long idUDFField;

	/** The id UDF parent field. */
	private Long idUDFParentField;

	/** The udf parent field value. */
	private String udfParentFieldValue;

	/** The field masc. */
	private String fieldMasc;

	/** The name. */
	private String name;

	/** The description. */
	private String description;

	/** The mandatory. */
	private Boolean mandatory;

	/** The field type. */
	private Integer fieldType;

	/** The field type label. */
	private String fieldTypeLabel;

	/** The id UDF list value. */
	private Long idUDFListValue;

	/** The enabled. */
	private Boolean enabled;

	/** The field list values DT os. */
	private List<UserDefinedFieldListValuesDTO> fieldListValuesDTOs = new ArrayList<>();

	/** The unique field. */
	private Boolean uniqueField;

	/** The names. */
	private List<String> names;

	/** The order. */
	private Integer ordre;

	/**
	 * Instantiates a new user defined fields.
	 */
	public UserDefinedFieldsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new user defined fields DTO.
	 *
	 * @param names the names
	 */
	public UserDefinedFieldsDTO(List<String> names) {

		this.names = names;
	}

	/**
	 * Instantiates a new user defined fields DTO.
	 *
	 * @param id the id
	 */
	public UserDefinedFieldsDTO(Long id) {

		this.id = id;
	}

	/**
	 * Instantiates a new user defined fields DTO.
	 *
	 * @param userDefinedFieldGroupDTO the user defined field group DTO
	 */
	public UserDefinedFieldsDTO(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO) {

		this.userDefinedFieldGroupDTO = userDefinedFieldGroupDTO;
	}

	/**
	 * Instantiates a new user defined fields DTO.
	 *
	 * @param userDefinedFieldGroupDTO the user defined field group DTO
	 * @param idUDFField the id UDF field
	 * @param idUDFParentField the id UDF parent field
	 * @param udfParentFieldValue the udf parent field value
	 * @param fieldMasc the field masc
	 * @param name the name
	 * @param description the description
	 * @param mandatory the mandatory
	 * @param fieldType the field type
	 * @param idUDFListValue the id UDF list value
	 * @param enabled the enabled
	 * @param uniqueField the unique field
	 * @param ordre the ordre
	 */
	public UserDefinedFieldsDTO(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO, Long idUDFField,
			Long idUDFParentField, String udfParentFieldValue, String fieldMasc, String name,
			String description, Boolean mandatory, Integer fieldType, Long idUDFListValue,
			Boolean enabled, Boolean uniqueField, Integer ordre) {

		this.userDefinedFieldGroupDTO = userDefinedFieldGroupDTO;
		this.idUDFField = idUDFField;
		this.idUDFParentField = idUDFParentField;
		this.udfParentFieldValue = udfParentFieldValue;
		this.fieldMasc = fieldMasc;
		this.name = name;
		this.description = description;
		this.mandatory = mandatory;
		this.fieldType = fieldType;
		this.idUDFListValue = idUDFListValue;
		this.enabled = enabled;
		this.uniqueField = uniqueField;
		this.ordre = ordre;
	}

	/**
	 * Instantiates a new user defined fields DTO.
	 *
	 * @param userDefinedFieldGroupDTO the user defined field group DTO
	 * @param idUDFField the id UDF field
	 * @param idUDFParentField the id UDF parent field
	 * @param udfParentFieldValue the udf parent field value
	 * @param fieldMasc the field masc
	 * @param name the name
	 * @param description the description
	 * @param mandatory the mandatory
	 * @param fieldType the field type
	 * @param idUDFListValue the id UDF list value
	 * @param enabled the enabled
	 * @param uniqueField the unique field
	 */
	public UserDefinedFieldsDTO(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO, Long idUDFField,
			Long idUDFParentField, String udfParentFieldValue, String fieldMasc, String name,
			String description, Boolean mandatory, Integer fieldType, Long idUDFListValue,
			Boolean enabled, Boolean uniqueField) {

		this.userDefinedFieldGroupDTO = userDefinedFieldGroupDTO;
		this.idUDFField = idUDFField;
		this.idUDFParentField = idUDFParentField;
		this.udfParentFieldValue = udfParentFieldValue;
		this.fieldMasc = fieldMasc;
		this.name = name;
		this.description = description;
		this.mandatory = mandatory;
		this.fieldType = fieldType;
		this.idUDFListValue = idUDFListValue;
		this.enabled = enabled;
		this.uniqueField = uniqueField;
	}

	/**
	 * Instantiates a new user defined fields DTO (USED IN ABACUS API SERVICE).
	 *
	 * @param userDefinedFieldGroupDTO the user defined field group DTO
	 * @param idUDFField the id UDF field
	 * @param name the name
	 */
	public UserDefinedFieldsDTO(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO, Long idUDFField,
			String name) {

		this.userDefinedFieldGroupDTO = userDefinedFieldGroupDTO;
		this.idUDFField = idUDFField;
		this.name = name;
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
	 * Gets the user defined field group DTO.
	 *
	 * @return the userDefinedFieldGroupDTO
	 */
	public UserDefinedFieldGroupDTO getUserDefinedFieldGroupDTO() {

		return userDefinedFieldGroupDTO;
	}

	/**
	 * Sets the user defined field group DTO.
	 *
	 * @param userDefinedFieldGroupDTO the userDefinedFieldGroupDTO to set
	 */
	public void setUserDefinedFieldGroupDTO(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO) {

		this.userDefinedFieldGroupDTO = userDefinedFieldGroupDTO;
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
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the field type label.
	 *
	 * @return the fieldTypeLabel
	 */
	public String getFieldTypeLabel() {

		if (!ACMValidationUtils.isNullOrEmpty(fieldType)) {
			return SettingUDFFieldsType.typeName(fieldType);
		}
		return fieldTypeLabel;
	}

	/**
	 * Sets the field type label.
	 *
	 * @param fieldTypeLabel the fieldTypeLabel to set
	 */
	public void setFieldTypeLabel(String fieldTypeLabel) {

		this.fieldTypeLabel = fieldTypeLabel;
	}

	/**
	 * Gets the field List Values DTOs.
	 * 
	 * @return the fieldListValuesDTOs
	 */
	public List<UserDefinedFieldListValuesDTO> getFieldListValuesDTOs() {

		return fieldListValuesDTOs;
	}

	/**
	 * Sets the field List Values DTOs.
	 * 
	 * @param fieldListValuesDTOs the fieldListValuesDTOs to set
	 */
	public void setFieldListValuesDTOs(List<UserDefinedFieldListValuesDTO> fieldListValuesDTOs) {

		this.fieldListValuesDTOs = fieldListValuesDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UserDefinedFieldsDTO [userDefinedFieldGroupDTO=" + userDefinedFieldGroupDTO
				+ ", idUDFField=" + idUDFField + ", name=" + name + ", idUDFListValue="
				+ idUDFListValue + "]";
	}

	/**
	 * Gets the names.
	 *
	 * @return the names
	 */
	public List<String> getNames() {

		return names;
	}

	/**
	 * Sets the names.
	 *
	 * @param names the names to set
	 */
	public void setNames(List<String> names) {

		this.names = names;
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

}
