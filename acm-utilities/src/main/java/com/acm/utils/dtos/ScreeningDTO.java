/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * {@link ScreeningDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
public class ScreeningDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6829739652146573534L;

	/** The customer DTO. */
	private CustomerDTO customerDTO;

	/** The customer category => CUSTOMER / GUARANTOR. */
	private String customerCategory;

	/** The id loan. */
	private Long idLoan;

	/** The loan DTO. */
	private LoanDTO loanDTO;

	/** The response aml data DT os. */
	private List<AMLDataDTO> responseAmlDataDTOs;

	/** The decision. */
	private String decision;

	/** The xml request. */
	private String xmlRequest;

	/** The xml response. */
	private String xmlResponse;

	/** The third party historique DTO. */
	private ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO;

	/** The score. */
	// Setting التقييم الرقمي
	private String score;

	/** The active loan. */
	// Setting عدد التسهيلات السارية
	private String activeLoan;

	/** The max num days due. */
	// Setting اقصى مدة تأخير للعميل
	private String maxNumDaysDue;

	/** The total approval amt. */
	private String totalApprovalAmt;

	/** The total monthly installment amt. */
	private String totalMonthlyInstallmentAmt;

	/** The total balance amount. */
	private String totalBalanceAmount;

	/** The iscore report. */
	private byte[] iscoreReport;

	/**
	 * Instantiates a new screening DTO.
	 */
	public ScreeningDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new screening DTO.
	 *
	 * @param xmlResponse the xml response
	 */
	public ScreeningDTO(String xmlResponse) {

		this.xmlResponse = xmlResponse;
	}

	/**
	 * Gets the customer DTO.
	 *
	 * @return the customerDTO
	 */
	public CustomerDTO getCustomerDTO() {

		return customerDTO;
	}

	/**
	 * Sets the customer DTO.
	 *
	 * @param customerDTO the customerDTO to set
	 */
	public void setCustomerDTO(CustomerDTO customerDTO) {

		this.customerDTO = customerDTO;
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
	 * Gets the customer category.
	 *
	 * @return the customerCategory
	 */
	public String getCustomerCategory() {

		return customerCategory;
	}

	/**
	 * Sets the customer category.
	 *
	 * @param customerCategory the customerCategory to set
	 */
	public void setCustomerCategory(String customerCategory) {

		this.customerCategory = customerCategory;
	}

	/**
	 * Gets the loan DTO.
	 *
	 * @return the loanDTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan DTO.
	 *
	 * @param loanDTO the loanDTO to set
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
	}

	/**
	 * Gets the response aml data DT os.
	 *
	 * @return the responseAmlDataDTOs
	 */
	public List<AMLDataDTO> getResponseAmlDataDTOs() {

		return responseAmlDataDTOs;
	}

	/**
	 * Sets the response aml data DT os.
	 *
	 * @param responseAmlDataDTOs the responseAmlDataDTOs to set
	 */
	public void setResponseAmlDataDTOs(List<AMLDataDTO> responseAmlDataDTOs) {

		this.responseAmlDataDTOs = responseAmlDataDTOs;
	}

	/**
	 * Gets the decision.
	 *
	 * @return the decision
	 */
	public String getDecision() {

		return decision;
	}

	/**
	 * Sets the decision.
	 *
	 * @param decision the decision to set
	 */
	public void setDecision(String decision) {

		this.decision = decision;
	}

	/**
	 * Gets the xml request.
	 *
	 * @return the xmlRequest
	 */
	public String getXmlRequest() {

		return xmlRequest;
	}

	/**
	 * Sets the xml request.
	 *
	 * @param xmlRequest the xmlRequest to set
	 */
	public void setXmlRequest(String xmlRequest) {

		this.xmlRequest = xmlRequest;
	}

	/**
	 * Gets the xml response.
	 *
	 * @return the xmlResponse
	 */
	public String getXmlResponse() {

		return xmlResponse;
	}

	/**
	 * Sets the xml response.
	 *
	 * @param xmlResponse the xmlResponse to set
	 */
	public void setXmlResponse(String xmlResponse) {

		this.xmlResponse = xmlResponse;
	}

	/**
	 * Gets the third party historique DTO.
	 *
	 * @return the thirdPartyHistoriqueDTO
	 */
	public ThirdPartyHistoriqueDTO getThirdPartyHistoriqueDTO() {

		return thirdPartyHistoriqueDTO;
	}

	/**
	 * Sets the third party historique DTO.
	 *
	 * @param thirdPartyHistoriqueDTO the thirdPartyHistoriqueDTO to set
	 */
	public void setThirdPartyHistoriqueDTO(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) {

		this.thirdPartyHistoriqueDTO = thirdPartyHistoriqueDTO;
	}

	/**
	 * Gets the score.
	 *
	 * @return the score
	 */
	public String getScore() {

		return score;
	}

	/**
	 * Sets the score.
	 *
	 * @param score the score to set
	 */
	public void setScore(String score) {

		this.score = score;
	}

	/**
	 * Gets the active loan.
	 *
	 * @return the activeLoan
	 */
	public String getActiveLoan() {

		return activeLoan;
	}

	/**
	 * Sets the active loan.
	 *
	 * @param activeLoan the activeLoan to set
	 */
	public void setActiveLoan(String activeLoan) {

		this.activeLoan = activeLoan;
	}

	/**
	 * Gets the max num days due.
	 *
	 * @return the maxNumDaysDue
	 */
	public String getMaxNumDaysDue() {

		return maxNumDaysDue;
	}

	/**
	 * Sets the max num days due.
	 *
	 * @param maxNumDaysDue the maxNumDaysDue to set
	 */
	public void setMaxNumDaysDue(String maxNumDaysDue) {

		this.maxNumDaysDue = maxNumDaysDue;
	}

	/**
	 * Gets the total approval amt.
	 *
	 * @return the totalApprovalAmt
	 */
	public String getTotalApprovalAmt() {

		return totalApprovalAmt;
	}

	/**
	 * Sets the total approval amt.
	 *
	 * @param totalApprovalAmt the totalApprovalAmt to set
	 */
	public void setTotalApprovalAmt(String totalApprovalAmt) {

		this.totalApprovalAmt = totalApprovalAmt;
	}

	/**
	 * Gets the total monthly installment amt.
	 *
	 * @return the totalMonthlyInstallmentAmt
	 */
	public String getTotalMonthlyInstallmentAmt() {

		return totalMonthlyInstallmentAmt;
	}

	/**
	 * Sets the total monthly installment amt.
	 *
	 * @param totalMonthlyInstallmentAmt the totalMonthlyInstallmentAmt to set
	 */
	public void setTotalMonthlyInstallmentAmt(String totalMonthlyInstallmentAmt) {

		this.totalMonthlyInstallmentAmt = totalMonthlyInstallmentAmt;
	}

	/**
	 * Gets the total balance amount.
	 *
	 * @return the totalBalanceAmount
	 */
	public String getTotalBalanceAmount() {

		return totalBalanceAmount;
	}

	/**
	 * Sets the total balance amount.
	 *
	 * @param totalBalanceAmount the totalBalanceAmount to set
	 */
	public void setTotalBalanceAmount(String totalBalanceAmount) {

		this.totalBalanceAmount = totalBalanceAmount;
	}

	/**
	 * Gets the iscore report.
	 *
	 * @return the iscore report
	 */
	public byte[] getIscoreReport() {

		return iscoreReport;
	}

	/**
	 * Sets the iscore report.
	 *
	 * @param iscoreReport the new iscore report
	 */
	public void setIscoreReport(byte[] iscoreReport) {

		this.iscoreReport = iscoreReport;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ScreeningDTO [" + (customerDTO != null ? "customerDTO=" + customerDTO + ", " : "")
				+ (customerCategory != null ? "customerCategory=" + customerCategory + ", " : "")
				+ (idLoan != null ? "idLoan=" + idLoan + ", " : "")
				+ (loanDTO != null ? "loanDTO=" + loanDTO + ", " : "")
				+ (responseAmlDataDTOs != null ? "responseAmlDataDTOs=" + responseAmlDataDTOs + ", "
						: "")
				+ (decision != null ? "decision=" + decision + ", " : "")
				+ (xmlRequest != null ? "xmlRequest=" + xmlRequest + ", " : "")
				+ (xmlResponse != null ? "xmlResponse=" + xmlResponse + ", " : "")
				+ (thirdPartyHistoriqueDTO != null
						? "thirdPartyHistoriqueDTO=" + thirdPartyHistoriqueDTO + ", "
						: "")
				+ (score != null ? "score=" + score + ", " : "")
				+ (activeLoan != null ? "activeLoan=" + activeLoan + ", " : "")
				+ (maxNumDaysDue != null ? "maxNumDaysDue=" + maxNumDaysDue + ", " : "")
				+ (totalApprovalAmt != null ? "totalApprovalAmt=" + totalApprovalAmt + ", " : "")
				+ (totalMonthlyInstallmentAmt != null
						? "totalMonthlyInstallmentAmt=" + totalMonthlyInstallmentAmt + ", "
						: "")
				+ (totalBalanceAmount != null ? "totalBalanceAmount=" + totalBalanceAmount : "")
				+ "]";
	}
}
