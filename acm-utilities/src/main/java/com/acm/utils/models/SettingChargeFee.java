/*
 * 
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The Class SettingChargeFee.
 */
@Entity
@Table(name = "ACM_SETTING_CHARGE_FEE")
public class SettingChargeFee extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4753582441889466219L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_CHARGE_FEE", unique = true, nullable = false)
	private Long id;

	/** The cufee id. */
	@Column(name = "ID_ABACUS", nullable = false)
	private Integer cufeeId;

	/** The label. */
	@Column(name = "LABEL")
	private String label;

	/** The code. */
	@Column(name = "CODE")
	private String code;

	/** The value. */
	@Column(name = "VALUE")
	private String value;

	/** The amount. */
	@Column(name = "AMOUNT")
	private BigDecimal amount;

	/** The percentage. */
	@Column(name = "PERCENTAGE")
	private int percentage;

	/** The id loan extern. */
	@Column(name = "ID_LOAN_EXTERN")
	private Long idLoanExtern;

	/** The id collection. */
	@Column(name = "ACM_ID_COLLECTION")
	private Long idCollection;

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

}
