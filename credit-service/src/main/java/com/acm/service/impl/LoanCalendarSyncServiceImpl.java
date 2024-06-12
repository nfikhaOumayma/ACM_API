package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.acm.client.CrmClient;
import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.service.AcmLoanInstanceAcmGroupeApprovalService;
import com.acm.service.LoanCalendarSyncService;
import com.acm.service.NotificationsServices;
import com.acm.utils.dtos.AcmLoanInstanceAcmGroupeApprovalDTO;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanInstanceDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;
import com.acm.utils.enums.NotificationStatut;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * The Class LoanCalendarSyncServiceImpl.
 */
@Service
public class LoanCalendarSyncServiceImpl implements LoanCalendarSyncService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanCalendarSyncServiceImpl.class);

	/**
	 * Instantiates a new loan calendar sync service impl.
	 *
	 * @param acmLoanInstanceGroupeAssociationService the acm loan instance groupe association service
	 * @param notificationsServices the notifications services
	 * @param userClient the user client
	 * @param parametrageClient the parametrage client
	 * @param crmClient the crm client
	 */
	public LoanCalendarSyncServiceImpl(
			AcmLoanInstanceAcmGroupeApprovalService acmLoanInstanceGroupeAssociationService,
			NotificationsServices notificationsServices, UserClient userClient,
			ParametrageClient parametrageClient, CrmClient crmClient) {

		super();
		this.acmLoanInstanceGroupeAssociationService = acmLoanInstanceGroupeAssociationService;
		this.notificationsServices = notificationsServices;
		this.userClient = userClient;
		this.parametrageClient = parametrageClient;
		this.crmClient = crmClient;
	}

	/** The acm loan instance groupe association service. */
	private AcmLoanInstanceAcmGroupeApprovalService acmLoanInstanceGroupeAssociationService;

	/** The notifications services. */

	private NotificationsServices notificationsServices;

	/** The user Client client. */

	private UserClient userClient;

	/** The parametrage client. */

	private ParametrageClient parametrageClient;

	/** The crm client. */

	private CrmClient crmClient;

	/**
	 * Send notifications to participants.
	 *
	 * @param currentLoanInstance the current loan instance
	 * @param loanDTO the loan DTO
	 */
	@Async
	@Override
	public void sendNotificationsToParticipants(LoanInstanceDTO currentLoanInstance,
			LoanDTO loanDTO) {

		List<AcmLoanInstanceAcmGroupeApprovalDTO> listValidatorsOfNextStep = new ArrayList<>();

		AcmLoanInstanceAcmGroupeApprovalDTO loanInstanceGroupeDTO =
				new AcmLoanInstanceAcmGroupeApprovalDTO();
		// Get list validators of next step
		LoanInstanceDTO currentLoansInstance = new LoanInstanceDTO();
		currentLoansInstance.setId(currentLoanInstance.getId() + 1);
		loanInstanceGroupeDTO.setValidation(false);
		loanInstanceGroupeDTO.setLoanInstance(currentLoansInstance);
		listValidatorsOfNextStep =
				acmLoanInstanceGroupeAssociationService.find(loanInstanceGroupeDTO);

		if (!ACMValidationUtils.isNullOrEmpty(listValidatorsOfNextStep)) {
			for (int j = 0; j < listValidatorsOfNextStep.size(); j++) {
				// prepare msg
				String actionDescription = "Notification participants approval";

				if (listValidatorsOfNextStep.get(j).getOwner() == null) {
					UserDTO userParticipantDTO = new UserDTO();
					userParticipantDTO
							.setGroupeCode(listValidatorsOfNextStep.get(j).getGroupeCode());
					// find Users
					List<UserDTO> listUsers = userClient.findByGroupe(userParticipantDTO);

					// Parccour list of users and send notification to users
					for (int k = 0; k < listUsers.size(); k++) {
						NotificationsDTO notificationsDTO = new NotificationsDTO();
						notificationsDTO = notificationsServices.save(new NotificationsDTO(
								listUsers.get(k).getLogin(), CommonConstants.LOAN_CATEGORY,
								NotificationType.INFO.name(), Boolean.TRUE,
								NotificationStatut.NEW.name(), actionDescription, loanDTO, null));
						logger.info("Notification Loan [{}] has been inserted.", notificationsDTO);
					}
				}
				else {

					NotificationsDTO notificationsDTO2 = new NotificationsDTO();
					notificationsDTO2 = notificationsServices
							.save(new NotificationsDTO(listValidatorsOfNextStep.get(j).getOwner(),
									CommonConstants.LOAN_CATEGORY, NotificationType.INFO.name(),
									Boolean.TRUE, NotificationStatut.NEW.name(), actionDescription,
									loanDTO, null));
					logger.info("Notification Loan [{}] has been inserted.", notificationsDTO2);
				}

			}
		}

		// find setting of next step
		WorkFlowStepDTO workFlowStepDTO = new WorkFlowStepDTO();
		workFlowStepDTO.setIdWorkFlowStep(currentLoanInstance.getCode().longValue() + 1);
		workFlowStepDTO.setProductId(loanDTO.getProductId());
		List<WorkFlowStepDTO> newWorkFlowStepDTOs =
				parametrageClient.findWorkFlowSteps(workFlowStepDTO);

		if (!ACMValidationUtils.isNullOrEmpty(newWorkFlowStepDTOs.get(0).getParticipants())) {
			// Parrcours list of Participants Loans
			for (int j = 0; j < newWorkFlowStepDTOs.get(0).getParticipants().size(); j++) {
				// creation instance of userParticipantDTO
				UserDTO userParticipantDTO = new UserDTO();
				// Set param GroupeCode to find users
				userParticipantDTO.setGroupeCode(
						newWorkFlowStepDTOs.get(0).getParticipants().get(j).getCode());
				// find Users
				List<UserDTO> listUsers = userClient.findByGroupe(userParticipantDTO);
				// prepare msg
				String actionDescription = "Notification participants";
				// Parccour list of users and send notification to users
				for (int k = 0; k < listUsers.size(); k++) {
					NotificationsDTO notificationsDTO = new NotificationsDTO();
					notificationsDTO = notificationsServices.save(new NotificationsDTO(
							listUsers.get(k).getLogin(), CommonConstants.LOAN_CATEGORY,
							NotificationType.INFO.name(), Boolean.TRUE,
							NotificationStatut.NEW.name(), actionDescription, loanDTO, null));

					logger.info("Notification Loan [{}] has been inserted.", notificationsDTO);
				}
			}
		}

	}

	/**
	 * Generation task for step loan.
	 *
	 * @param loanDTO the loan DTO
	 * @param idStepWorkFlow the id step work flow
	 * @param nextWorkFlowStepDTO the next work flow step DTO
	 */
	@Async
	@Override
	public void generationTaskForStepLoan(LoanDTO loanDTO, Long idStepWorkFlow,
			WorkFlowStepDTO nextWorkFlowStepDTO) {

		try {

			CalendarEventDTO newCalendarTaskDTO = new CalendarEventDTO();
			Calendar calendar = Calendar.getInstance();
			Boolean caseAssginUserLoanToHimSelf = false;
			if (ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTO)
					&& !ACMValidationUtils.isNullOrEmpty(idStepWorkFlow)) {
				// Get workflow step setting of the next step
				WorkFlowStepDTO nextWorkFlowStepDTOParam = new WorkFlowStepDTO();
				nextWorkFlowStepDTOParam.setIdWorkFlowStep(idStepWorkFlow);
				nextWorkFlowStepDTOParam.setProductId(loanDTO.getProductId());
				nextWorkFlowStepDTO =
						parametrageClient.findWorkFlowSteps(nextWorkFlowStepDTOParam).get(0);
			}
			newCalendarTaskDTO
					.setCustomerNumber(loanDTO.getCustomerDTO().getCustomerNumber().toString());
			newCalendarTaskDTO.setCustomerName(loanDTO.getCustomerName().replaceAll("\\|", " "));
			// if next step don't have generation task activate so we must only close previous task
			if (!ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTO)
					&& !Boolean.TRUE.equals(nextWorkFlowStepDTO.getGenerationTask())) {
				Boolean createTask = Boolean.FALSE;
				crmClient.createLoanTaskAndCloseOldTask(newCalendarTaskDTO, createTask,
						caseAssginUserLoanToHimSelf, Boolean.FALSE);
			}

			// check if generation task of the next step in settingWF is activate
			if (!ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTO)
					&& Boolean.TRUE.equals(nextWorkFlowStepDTO.getGenerationTask())
					&& (loanDTO.getOwner() != null || loanDTO.getGroupOwner() != null)) {
				Boolean createTask = Boolean.TRUE;
				// fill task data
				newCalendarTaskDTO.setLibelleEvent(loanDTO.getStatutLibelle());
				newCalendarTaskDTO.setDescription(loanDTO.getStatutLibelle());
				calendar.set(Calendar.HOUR_OF_DAY, 8); // 24-hour format
				calendar.set(Calendar.MINUTE, 0);
				calendar.set(Calendar.SECOND, 0);
				calendar.set(Calendar.MILLISECOND, 0);
				newCalendarTaskDTO.setDateDebut(calendar.getTime());
				calendar.set(Calendar.HOUR_OF_DAY, 18);
				newCalendarTaskDTO.setDateFin(calendar.getTime());
				newCalendarTaskDTO.setTypeEvent(ACMConstantWorkflowStatuts.NEXT_ACTION_TASK);
				String customerNameNoPipe = (loanDTO.getCustomerName()).replaceAll("\\|", " ");
				newCalendarTaskDTO.setCustomerName(customerNameNoPipe);
				newCalendarTaskDTO.setIdLoanExtern(loanDTO.getIdLoanExtern());
				newCalendarTaskDTO.setCategory(CommonConstants.LOAN_CATEGORY);
				newCalendarTaskDTO
						.setIdCustomerExtern(loanDTO.getCustomerDTO().getCustomerIdExtern());

				if (loanDTO.getOwner() != null) {

					newCalendarTaskDTO.setUsername(loanDTO.getOwner());
					newCalendarTaskDTO.setUserEmail(loanDTO.getOwnerEmail());
					crmClient.createLoanTaskAndCloseOldTask(newCalendarTaskDTO, createTask,
							caseAssginUserLoanToHimSelf, Boolean.FALSE);
				}

				else {
					// get list users of group
					GroupeDTO groupeDTO =
							parametrageClient.findGroupeByCode(loanDTO.getGroupOwner());
					// get the list of users that have loanBranch in their accessBranch
					List<UserDTO> userDTOParam =
							userClient.getUsersWithLoanBranchInTheirAccessBranches(
									loanDTO.getBranchID(), groupeDTO.getUserDTOs());
					// if the group has only one user(base on access branch)
					// then assign to this user
					if (userDTOParam.size() == 1) {
						newCalendarTaskDTO.setUsername(userDTOParam.get(0).getLogin());
						newCalendarTaskDTO.setUserEmail(userDTOParam.get(0).getEmail());
						crmClient.createLoanTaskAndCloseOldTask(newCalendarTaskDTO, createTask,
								caseAssginUserLoanToHimSelf, Boolean.FALSE);
					}

					else {
						// loop list of users and send for each user a task
						for (int i = 0; i < userDTOParam.size(); i++) {
							newCalendarTaskDTO.setUsername(userDTOParam.get(i).getLogin());
							newCalendarTaskDTO.setUserEmail(userDTOParam.get(i).getEmail());
							if (i == 0) {
								// first iteration: for close old tasks and create new task
								crmClient.createLoanTaskAndCloseOldTask(newCalendarTaskDTO,
										createTask, caseAssginUserLoanToHimSelf, Boolean.FALSE);
							}
							// from the second iteration: we only continue to create new tasks
							if (i > 0) {
								crmClient.create(newCalendarTaskDTO);
							}
						}
					}

				}
			}
		}
		catch (Exception e) {
			logger.error("Error Creating/Updating Loan Tasks : {}", e.getMessage());
		}

	}

	/**
	 * Generation task for approval participants.
	 *
	 * @param loanDTO the loan DTO
	 * @param idStepWorkFlow the id step work flow
	 * @param nextWorkFlowStepDTO the next work flow step DTO
	 * @param idLoanInstance the id loan instance
	 */
	@Async
	@Override
	public void generationTaskForApprovalParticipants(LoanDTO loanDTO, Long idStepWorkFlow,
			WorkFlowStepDTO nextWorkFlowStepDTO, Long idLoanInstance) {

		try {

			CalendarEventDTO newCalendarTaskDTO = new CalendarEventDTO();
			Calendar calendar = Calendar.getInstance();
			if (ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTO)
					&& !ACMValidationUtils.isNullOrEmpty(idStepWorkFlow)) {
				// Get workflow step setting of the next step
				WorkFlowStepDTO nextWorkFlowStepDTOParam = new WorkFlowStepDTO();
				nextWorkFlowStepDTOParam.setIdWorkFlowStep(idStepWorkFlow);
				nextWorkFlowStepDTOParam.setProductId(loanDTO.getProductId());
				nextWorkFlowStepDTO =
						parametrageClient.findWorkFlowSteps(nextWorkFlowStepDTOParam).get(0);
			}
			newCalendarTaskDTO
					.setCustomerNumber(loanDTO.getCustomerDTO().getCustomerNumber().toString());
			newCalendarTaskDTO.setCustomerName(loanDTO.getCustomerName().replaceAll("\\|", " "));

			// check if we have validators in the next step in settingWF
			if (!ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTO)
					&& !ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTO.getApprovers())) {
				// fill task data
				newCalendarTaskDTO.setLibelleEvent(loanDTO.getStatutLibelle());
				newCalendarTaskDTO.setDescription(loanDTO.getStatutLibelle());
				calendar.set(Calendar.HOUR_OF_DAY, 8); // 24-hour format
				calendar.set(Calendar.MINUTE, 0);
				calendar.set(Calendar.SECOND, 0);
				calendar.set(Calendar.MILLISECOND, 0);
				newCalendarTaskDTO.setDateDebut(calendar.getTime());
				calendar.set(Calendar.HOUR_OF_DAY, 18);
				newCalendarTaskDTO.setDateFin(calendar.getTime());
				newCalendarTaskDTO.setTypeEvent(ACMConstantWorkflowStatuts.NEXT_ACTION_TASK);
				String customerNameNoPipe = (loanDTO.getCustomerName()).replaceAll("\\|", " ");
				newCalendarTaskDTO.setCustomerName(customerNameNoPipe);
				newCalendarTaskDTO.setIdLoanExtern(loanDTO.getIdLoanExtern());
				newCalendarTaskDTO.setCategory(CommonConstants.LOAN_CATEGORY);
				newCalendarTaskDTO
						.setIdCustomerExtern(loanDTO.getCustomerDTO().getCustomerIdExtern());

				// find list validators
				AcmLoanInstanceAcmGroupeApprovalDTO loanInstanceGroupeApprovalParam =
						new AcmLoanInstanceAcmGroupeApprovalDTO();

				LoanInstanceDTO currentLoansInstance = new LoanInstanceDTO();
				currentLoansInstance.setId(idLoanInstance);
				loanInstanceGroupeApprovalParam.setValidation(false);
				loanInstanceGroupeApprovalParam.setLoanInstance(currentLoansInstance);
				List<AcmLoanInstanceAcmGroupeApprovalDTO> listAcmLoanInstanceAcmGroupeApprovalDTO =
						acmLoanInstanceGroupeAssociationService
								.find(loanInstanceGroupeApprovalParam);

				if (!ACMValidationUtils.isNullOrEmpty(listAcmLoanInstanceAcmGroupeApprovalDTO)) {
					for (AcmLoanInstanceAcmGroupeApprovalDTO loanInstanceApprovalDTO : listAcmLoanInstanceAcmGroupeApprovalDTO) {
						if (loanInstanceApprovalDTO.getOwner() != null) {

							newCalendarTaskDTO.setUsername(loanInstanceApprovalDTO.getOwner());
							newCalendarTaskDTO.setUserEmail(loanDTO.getOwnerEmail());
							crmClient.create(newCalendarTaskDTO);
						}
						else {
							// get list users of group
							GroupeDTO groupeDTO = parametrageClient
									.findGroupeByCode(loanInstanceApprovalDTO.getGroupeCode());
							// get the list of users that have loanBranch in their accessBranch
							List<UserDTO> userDTOParam =
									userClient.getUsersWithLoanBranchInTheirAccessBranches(
											loanDTO.getBranchID(), groupeDTO.getUserDTOs());
							// if the group has only one user(base on access branch)
							if (userDTOParam.size() == 1) {
								newCalendarTaskDTO.setUsername(userDTOParam.get(0).getLogin());
								newCalendarTaskDTO.setUserEmail(userDTOParam.get(0).getEmail());
								crmClient.create(newCalendarTaskDTO);
							}
							else {
								// loop list of users and send for each user a task
								for (int i = 0; i < userDTOParam.size(); i++) {
									newCalendarTaskDTO.setUsername(userDTOParam.get(i).getLogin());
									newCalendarTaskDTO.setUserEmail(userDTOParam.get(i).getEmail());
									crmClient.create(newCalendarTaskDTO);
								}
							}
						}
					}
				}
			}
		}
		catch (Exception e) {
			logger.error("Error Creating/Updating Loan Tasks : {}", e.getMessage());
		}

	}
}
