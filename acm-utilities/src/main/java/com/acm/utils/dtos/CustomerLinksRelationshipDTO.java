/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;

import org.dozer.Mapping;

/**
 * The Class CustomerLinksRelationshipDTO.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
public class CustomerLinksRelationshipDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6395941092163281673L;

	/** The id. */
	private Long id;

	/** The customer id. */
	private Long customerId;

	/** The member. */
	@Mapping("member")
	private CustomerDTO member;

	/** The link relationship type. */
	private String linkRelationshipType;

	/** The category. */
	private String category;

	/** The date debut. */
	private Date dateDebut;

	/** The date fin. */
	private Date dateFin;

	/** The idloan. */
	private Long idLoan;

	/** The amount guarantor. */
	private BigDecimal amountGuarantor;

	/** The force search. */
	private Boolean forceSearch;

	/** The percentage owned. */
	private BigDecimal percentageOwned;

	/** The check for group. */
	private Boolean checkForGroup;

	/** The enabled. */
	private Boolean enabled;

	/** The loans DTO. */
	private Long[] loanIds;

	/** The loan DTO. */
	private LoanDTO loanDTO;

	/** The statut loan. */
	private Integer statutLoan;

	/**
	 * Instantiates a new customer links relationship DTO.
	 */
	public CustomerLinksRelationshipDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new customer links relationship DTO.
	 *
	 * @param category the category
	 * @param forceSearch the force search
	 */
	public CustomerLinksRelationshipDTO(String category, Boolean forceSearch) {

		this.category = category;
		this.forceSearch = forceSearch;
	}

	/**
	 * Instantiates a new customer links relationship DTO.
	 *
	 * @param id the id
	 * @param customerId the customer id
	 * @param member the member
	 * @param linkRelationshipType the link relationship type
	 * @param category the category
	 * @param dateDebut the date debut
	 * @param dateFin the date fin
	 * @param idLoan the idLoan
	 * @param amountGuarantor the amount guarantor
	 */
	public CustomerLinksRelationshipDTO(Long id, Long customerId, CustomerDTO member,
			String linkRelationshipType, String category, Date dateDebut, Date dateFin, Long idLoan,
			BigDecimal amountGuarantor) {

		this.id = id;
		this.customerId = customerId;
		this.member = member;
		this.linkRelationshipType = linkRelationshipType;
		this.category = category;
		this.dateDebut = dateDebut;
		this.dateFin = dateFin;
		this.idLoan = idLoan;
		this.amountGuarantor = amountGuarantor;

	}

	/**
	 * Instantiates a new customer links relationship DTO in SAVE LOAN Method.
	 *
	 * @param customerId the customer id
	 * @param member the member
	 * @param linkRelationshipType the link relationship type
	 * @param category the category
	 * @param dateDebut the date debut
	 * @param idLoan the id loan
	 * @param percentageOwned the percentage owned
	 */
	public CustomerLinksRelationshipDTO(Long customerId, CustomerDTO member,
			String linkRelationshipType, String category, Date dateDebut, Long idLoan,
			BigDecimal percentageOwned) {

		this.customerId = customerId;
		this.member = member;
		this.linkRelationshipType = linkRelationshipType;
		this.category = category;
		this.dateDebut = dateDebut;
		this.idLoan = idLoan;
		this.percentageOwned = percentageOwned;
	}

	/**
	 * Instantiates a new customer links relationship DTO.
	 *
	 * @param customerId the customer id
	 * @param member the member
	 * @param category the category
	 */
	public CustomerLinksRelationshipDTO(Long customerId, CustomerDTO member, String category) {

		this.customerId = customerId;
		this.member = member;
		this.category = category;
	}

	/**
	 * Instantiates a new customer links relationship DTO : USED in cancelGuarantorLoan().
	 *
	 * @param member the member
	 * @param idLoan the id loan
	 */
	public CustomerLinksRelationshipDTO(CustomerDTO member, Long idLoan) {

		this.member = member;
		this.idLoan = idLoan;
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
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the link relationship type.
	 *
	 * @return the linkRelationshipType
	 */
	public String getLinkRelationshipType() {

		return linkRelationshipType;
	}

	/**
	 * Sets the link relationship type.
	 *
	 * @param linkRelationshipType the linkRelationshipType to set
	 */
	public void setLinkRelationshipType(String linkRelationshipType) {

		this.linkRelationshipType = linkRelationshipType;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the date debut.
	 *
	 * @return the dateDebut
	 */
	public Date getDateDebut() {

		return dateDebut;
	}

	/**
	 * Sets the date debut.
	 *
	 * @param dateDebut the dateDebut to set
	 */
	public void setDateDebut(Date dateDebut) {

		this.dateDebut = dateDebut;
	}

	/**
	 * Gets the date fin.
	 *
	 * @return the dateFin
	 */
	public Date getDateFin() {

		return dateFin;
	}

	/**
	 * Sets the date fin.
	 *
	 * @param dateFin the dateFin to set
	 */
	public void setDateFin(Date dateFin) {

		this.dateFin = dateFin;
	}

	/**
	 * Gets the member.
	 *
	 * @return the member
	 */
	public CustomerDTO getMember() {

		return member;
	}

	/**
	 * Sets the member.
	 *
	 * @param member the member to set
	 */
	public void setMember(CustomerDTO member) {

		this.member = member;
	}

	/**
	 * Gets the id loan.
	 *
	 * @return the idLoan
	 */
	public Long getIdLoan() {

		return idLoan;
	}

	/**
	 * Sets the id loan.
	 *
	 * @param idLoan the idLoan to set
	 */
	public void setIdLoan(Long idLoan) {

		this.idLoan = idLoan;
	}

	/**
	 * Gets the amount guarantor.
	 *
	 * @return the amountGuarantor
	 */
	public BigDecimal getAmountGuarantor() {

		return amountGuarantor;
	}

	/**
	 * Sets the amount guarantor.
	 *
	 * @param amountGuarantor the amountGuarantor to set
	 */
	public void setAmountGuarantor(BigDecimal amountGuarantor) {

		this.amountGuarantor = amountGuarantor;
	}

	/**
	 * Gets the force search.
	 *
	 * @return the forceSearch
	 */
	public Boolean getForceSearch() {

		return forceSearch;
	}

	/**
	 * Sets the force search.
	 *
	 * @param forceSearch the forceSearch to set
	 */
	public void setForceSearch(Boolean forceSearch) {

		this.forceSearch = forceSearch;
	}

	/**
	 * Gets the percentage owned.
	 *
	 * @return the percentageOwned
	 */
	public BigDecimal getPercentageOwned() {

		return percentageOwned;
	}

	/**
	 * Sets the percentage owned.
	 *
	 * @param percentageOwned the percentageOwned to set
	 */
	public void setPercentageOwned(BigDecimal percentageOwned) {

		this.percentageOwned = percentageOwned;
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
	 * Gets the check for group.
	 *
	 * @return the check for group
	 */
	public Boolean getCheckForGroup() {

		return checkForGroup;
	}

	/**
	 * Sets the check for group.
	 *
	 * @param checkForGroup the new check for group
	 */
	public void setCheckForGroup(Boolean checkForGroup) {

		this.checkForGroup = checkForGroup;
	}

	/**
	 * Gets the loan ids.
	 *
	 * @return the loan ids
	 */
	public Long[] getLoanIds() {

		return loanIds;
	}

	/**
	 * Sets the loan ids.
	 *
	 * @param loanIds the new loan ids
	 */
	public void setLoanIds(Long[] loanIds) {

		this.loanIds = loanIds;
	}

	/**
	 * Gets the loan DTO.
	 *
	 * @return the loan DTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan DTO.
	 *
	 * @param loanDTO the new loan DTO
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
	}

	/**
	 * Gets the statut loan.
	 *
	 * @return the statut loan
	 */
	public Integer getStatutLoan() {

		return statutLoan;
	}

	/**
	 * Sets the statut loan.
	 *
	 * @param statutLoan the new statut loan
	 */
	public void setStatutLoan(Integer statutLoan) {

		this.statutLoan = statutLoan;
	}

}
