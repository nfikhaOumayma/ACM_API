/*
 * 
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * The Class SettingChargeFeeDTO.
 */
public class SettingChargeFeeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 369629612787052002L;

	/** The id. */
	private Long id;

	/** The cufee id. */
	private Integer cufeeId;

	/** The code. */
	private String code;

	/** The label. */
	private String label;

	/** The value. */
	private String value;

	/** The amount. */
	private BigDecimal amount;

	/** The percentage. */
	private int percentage;

	/** The date insert. */
	private Date dateInsertion;

	/** The enabled. */
	private Boolean enabled;

	/** The id collection. */
	private Long idCollection;

	/** The id loan extern. */
	private Long idLoanExtern;

	/** The insert by. */
	private String insertBy;

	/**
	 * Instantiates a new setting charge fee DTO.
	 */
	public SettingChargeFeeDTO() {

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
	 * Gets the cufee id.
	 *
	 * @return the cufee id
	 */
	public Integer getCufeeId() {

		return cufeeId;
	}

	/**
	 * Sets the cufee id.
	 *
	 * @param cufeeId the new cufee id
	 */
	public void setCufeeId(Integer cufeeId) {

		this.cufeeId = cufeeId;
	}

	/**
	 * Gets the label.
	 *
	 * @return the label
	 */
	public String getLabel() {

		return label;
	}

	/**
	 * Sets the label.
	 *
	 * @param label the new label
	 */
	public void setLabel(String label) {

		this.label = label;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the new code
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public String getValue() {

		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the new value
	 */
	public void setValue(String value) {

		this.value = value;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

	/**
	 * Gets the percentage.
	 *
	 * @return the percentage
	 */
	public int getPercentage() {

		return percentage;
	}

	/**
	 * Sets the percentage.
	 *
	 * @param percentage the new percentage
	 */
	public void setPercentage(int percentage) {

		this.percentage = percentage;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
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
	 * Gets the id collection.
	 *
	 * @return the id collection
	 */
	public Long getIdCollection() {

		return idCollection;
	}

	/**
	 * Sets the id collection.
	 *
	 * @param idCollection the new id collection
	 */
	public void setIdCollection(Long idCollection) {

		this.idCollection = idCollection;
	}

	/**
	 * Gets the id loan extern.
	 *
	 * @return the id loan extern
	 */
	public Long getIdLoanExtern() {

		return idLoanExtern;
	}

	/**
	 * Sets the id loan extern.
	 *
	 * @param idLoanExtern the new id loan extern
	 */
	public void setIdLoanExtern(Long idLoanExtern) {

		this.idLoanExtern = idLoanExtern;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insert by
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the new insert by
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

}
