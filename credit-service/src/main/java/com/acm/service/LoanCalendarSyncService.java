package com.acm.service;

import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanInstanceDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;

/**
 * The Interface LoanCalendarSyncService.
 */
public interface LoanCalendarSyncService {

	/**
	 * Generation task for approval participants.
	 *
	 * @param loanDTO the loan DTO
	 * @param idStepWorkFlow the id step work flow
	 * @param nextWorkFlowStepDTO the next work flow step DTO
	 * @param idLoanInstance the id loan instance
	 */
	void generationTaskForApprovalParticipants(LoanDTO loanDTO, Long idStepWorkFlow,
			WorkFlowStepDTO nextWorkFlowStepDTO, Long idLoanInstance);

	/**
	 * Generation task for step loan.
	 *
	 * @param loanDTO the loan DTO
	 * @param idStepWorkFlow the id step work flow
	 * @param nextWorkFlowStepDTO the next work flow step DTO
	 */
	void generationTaskForStepLoan(LoanDTO loanDTO, Long idStepWorkFlow,
			WorkFlowStepDTO nextWorkFlowStepDTO);

	/**
	 * Send notifications to participants.
	 *
	 * @param currentLoanInstance the current loan instance
	 * @param loanDTO the loan DTO
	 */
	void sendNotificationsToParticipants(LoanInstanceDTO currentLoanInstance, LoanDTO loanDTO);

}
