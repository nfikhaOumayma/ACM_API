package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;

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
 * The Class ChargeFees.
 */
@Entity
@Table(name = "ACM_CHARGE_FEES")
public class ChargeFees extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3143845128133127510L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_CHARGE_FEE", unique = true, nullable = false)
	private Long id;

	/** The cufee id. */
	@Column(name = "ID_ABACUS", nullable = false)
	private Integer cufeeId;

	/** The setting fee. */
	@Column(name = "ID_ACM_SETTING_CHARGE_FEE", nullable = false)
	private Long settingFee;

	/** The amount. */
	@Column(name = "AMOUNT")
	private BigDecimal amount;

	/** The label. */
	@Column(name = "LABEL")
	private String label;

	/** The code. */
	@Column(name = "CODE")
	private String code;

	/** The value. */
	@Column(name = "VALUE")
	private String value;

	/** The charged. */
	@Column(name = "CHARGED")
	private Boolean charged;

	/** The loan instance. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN_INSTANCE")
	private LoanInstance loanInstance;

	/** The collection instance. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_COLLECTION_INSTANCE")
	private CollectionInstance collectionInstance;

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
	 * Gets the setting fee.
	 *
	 * @return the setting fee
	 */
	public Long getSettingFee() {

		return settingFee;
	}

	/**
	 * Sets the setting fee.
	 *
	 * @param settingFee the new setting fee
	 */
	public void setSettingFee(Long settingFee) {

		this.settingFee = settingFee;
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
	 * Gets the charged.
	 *
	 * @return the charged
	 */
	public Boolean getCharged() {

		return charged;
	}

	/**
	 * Sets the charged.
	 *
	 * @param charged the new charged
	 */
	public void setCharged(Boolean charged) {

		this.charged = charged;
	}

	/**
	 * Gets the loan instance.
	 *
	 * @return the loan instance
	 */
	public LoanInstance getLoanInstance() {

		return loanInstance;
	}

	/**
	 * Sets the loan instance.
	 *
	 * @param loanInstance the new loan instance
	 */
	public void setLoanInstance(LoanInstance loanInstance) {

		this.loanInstance = loanInstance;
	}

	/**
	 * Gets the collection instance.
	 *
	 * @return the collection instance
	 */
	public CollectionInstance getCollectionInstance() {

		return collectionInstance;
	}

	/**
	 * Sets the collection instance.
	 *
	 * @param collectionInstance the new collection instance
	 */
	public void setCollectionInstance(CollectionInstance collectionInstance) {

		this.collectionInstance = collectionInstance;
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

}
