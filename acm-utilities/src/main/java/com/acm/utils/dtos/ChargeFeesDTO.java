package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

import org.dozer.Mapping;

/**
 * The Class ChargeFeesDTO.
 */
public class ChargeFeesDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6785980305229627774L;

	/** The id. */
	private Long id;

	/** The cufee id. */
	private Integer cufeeId;

	/** The setting fee. */
	private Long settingFee;

	/** The amount. */
	private BigDecimal amount;

	/** The charged. */
	private Boolean charged;

	/** The id loan instance. */
	@Mapping("loanInstance.id")
	private Long idLoanInstance;

	/** The id collection instance. */
	@Mapping("collectionInstance.id")
	private Long idCollectionInstance;

	/** The label. */
	private String label;

	/** The code. */
	private String code;

	/** The value. */
	private String value;

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
	 * Gets the id loan instance.
	 *
	 * @return the id loan instance
	 */
	public Long getIdLoanInstance() {

		return idLoanInstance;
	}

	/**
	 * Sets the id loan instance.
	 *
	 * @param idLoanInstance the new id loan instance
	 */
	public void setIdLoanInstance(Long idLoanInstance) {

		this.idLoanInstance = idLoanInstance;
	}

	/**
	 * Gets the id collection instance.
	 *
	 * @return the id collection instance
	 */
	public Long getIdCollectionInstance() {

		return idCollectionInstance;
	}

	/**
	 * Sets the id collection instance.
	 *
	 * @param idCollectionInstance the new id collection instance
	 */
	public void setIdCollectionInstance(Long idCollectionInstance) {

		this.idCollectionInstance = idCollectionInstance;
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
