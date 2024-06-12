/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.repository.LoanRepository;
import com.acm.service.LoanDetailsService;
import com.acm.service.LoanParticipantsService;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.FinancialAnalysisDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanDetailsDTO;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QLoan;
import com.acm.utils.models.QLoanParticipants;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.jpa.JPAExpressions;

/**
 * {@link LoanDetailsServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@Service
public class LoanDetailsServiceImpl implements LoanDetailsService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanDetailsServiceImpl.class);

	/** The loan repository. */
	@Autowired
	private LoanRepository loanRepository;

	/** The loan participants service. */
	@Autowired
	private LoanParticipantsService loanParticipantsService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findDetailsLoan(java.lang.Long)
	 */
	@Override
	public LoanDetailsDTO findDetailsLoan(Long idLoanExtern) {

		logger.info("findDetailsLoan by idLoanExtern = {}", idLoanExtern);
		LoanDetailsDTO loanDetailsDTO = new LoanDetailsDTO();
		// init loanDTO
		LoanDTO loanDTO = findLoanByIdLoanExtern(idLoanExtern);

		if (loanDTO != null) {
			// load Schedule data
			loanDetailsDTO.setScheduleDTOs(transversClient.findSchedules(idLoanExtern));

			// load CustomerAccount data
			loanDetailsDTO.setCustomerAccountDTOs(
					transversClient.findCustomerAccountByLoan(idLoanExtern));

			// load loan data from ACM
			Loan loanACM =
					loanRepository.findByIdLoanExternAndEnabled(idLoanExtern, Boolean.TRUE).get(0);
			// mapping data
			LoanDTO loanDTOABACUS = mapper.map(loanACM, LoanDTO.class);
			loanDetailsDTO.setLoanDTO(loanDTOABACUS);
		}
		return loanDetailsDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findDetailsCustomer(java.lang.Long)
	 */
	@Override
	public CustomerDTO findDetailsCustomer(Long idLoanExtern) {

		logger.info("findDetailsCustomer by idLoanExtern = {}", idLoanExtern);
		CustomerDTO customerDTO = new CustomerDTO();
		LoanDTO loanDTO = findLoanByIdLoanExtern(idLoanExtern);
		if (loanDTO != null) {
			customerDTO = transversClient.findCustomerByLoan(idLoanExtern);
		}
		return customerDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findGuarantors(java.lang.Long)
	 */
	@Override
	public List<GuarantorDTO> findGuarantors(Long idLoanExtern) {

		logger.info("findGuarantors by idLoanExtern = {}", idLoanExtern);
		List<GuarantorDTO> guarantorDTOs = new ArrayList<>();
		LoanDTO loanDTO = findLoanByIdLoanExtern(idLoanExtern);
		if (loanDTO != null) {
			guarantorDTOs = transversClient.findGuarantors(idLoanExtern);
		}
		return guarantorDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findCollaterols(java.lang.Long)
	 */
	@Override
	public List<CollaterolDTO> findCollaterols(Long idLoanExtern) {

		logger.info("findDetails by idLoanExtern = {}", idLoanExtern);
		List<CollaterolDTO> collaterolDTOs = new ArrayList<>();
		LoanDTO loanDTO = findLoanByIdLoanExtern(idLoanExtern);
		if (loanDTO != null) {
			collaterolDTOs = transversClient.findCollaterols(idLoanExtern);
		}
		return collaterolDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanDetailsService#findActiveAndInactiveCollaterols(java.lang.Long)
	 */
	@Override
	public List<AcmCollateralDTO> findActiveAndInactiveCollaterols(List<Long> idLoanExtern) {

		logger.info("findDetails by idLoanExtern = {}", idLoanExtern);
		// init list of idLoanExterns to send to transversClient
		List<Long> externIds = new ArrayList<>();
		List<AcmCollateralDTO> collaterolDTOs = new ArrayList<>();
		for (Long id : idLoanExtern) {
			LoanDTO loanDTO = findLoanByIdLoanExtern(id);
			if (loanDTO != null) {
				externIds.add(id);
			}
		}
		if (!ACMValidationUtils.isNullOrEmpty(externIds)) {
			collaterolDTOs = transversClient.findActiveAndInactiveCollaterols(externIds);
		}
		return collaterolDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findFinancialAnalysis(java.lang.Long)
	 */
	@Override
	public List<FinancialAnalysisDTO> findFinancialAnalysis(Long idLoanExtern) {

		logger.info("findFinancialAnalysis by idLoanExtern = {}", idLoanExtern);
		List<FinancialAnalysisDTO> financialAnalysisDTOs = new ArrayList<>();
		LoanDTO loanDTO = findLoanByIdLoanExtern(idLoanExtern);
		if (loanDTO != null) {
			financialAnalysisDTOs = transversClient.findFinancialAnalysis(idLoanExtern);
		}
		return financialAnalysisDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findCustomerAccountScheduleByLoan(java.lang.Long)
	 */
	@Override
	public List<ScheduleDTO> findCustomerAccountScheduleByLoan(Long idLoanExtern) {

		logger.info("findCustomerAccountSchedule by idLoanExtern = {}", idLoanExtern);
		return transversClient.findCustomerAccountScheduleByLoan(idLoanExtern);
	}

	/**
	 * Find loan by id loan extern if exist and connected user is permit to see the details.
	 * 
	 * @author HaythemBenizid
	 * @param idLoanExtern the id loan extern
	 * @return the loan DTO
	 */
	private LoanDTO findLoanByIdLoanExtern(Long idLoanExtern) {

		logger.info("findLoan by idLoanExtern = {}", idLoanExtern);
		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// init QLoan
		QLoan qLoan = QLoan.loan;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by owner/sub-owner and idLoanExtern
		predicate.and(qLoan.idLoanExtern.eq(idLoanExtern));

		// setting subPredicate to filter list participant by Id & owner
		BooleanBuilder subPredicate = new BooleanBuilder();
		QLoanParticipants qLoanParticipants = QLoanParticipants.loanParticipants;
		List<LoanParticipantsDTO> loanParticipantsDTOs =
				loanParticipantsService.find(new LoanParticipantsDTO(null, userDTO.getLogin()));
		if (loanParticipantsDTOs.size() <= 1000) {
			List<Long> wheresIds = new ArrayList<>();
			loanParticipantsDTOs
					.forEach(loanParticipantsDTO -> wheresIds.add(loanParticipantsDTO.getIdLoan()));
			subPredicate.and(qLoan.idLoan.in(new ArrayList<>(new HashSet<>(wheresIds))));
		}
		else {
			subPredicate.and(qLoan.idLoan.in(
					JPAExpressions.selectFrom(qLoanParticipants).select(qLoanParticipants.idLoan)
							.where(qLoanParticipants.username.eq(userDTO.getLogin()))));
		}

		// find all user responsable && collaborator
		List<String> wheresOwners = new ArrayList<>();
		List<UserDTO> userDTOs = userClient.findUsers();
		userDTOs.forEach(user -> {
			if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
				wheresOwners.add(user.getLogin());
			}
		});
		// setting predicate to find by Id
		subPredicate.or(qLoan.owner.in(new ArrayList<>(new HashSet<>(wheresOwners))));

		// find loan by Access Branches for connected user
		if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(",")).stream()
					.map(String::trim).mapToInt(Integer::parseInt).toArray();
			logger.info("ID Access Branches = {}", arrayBranchIds);
			List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length);
			for (int i : arrayBranchIds) {
				listBranchIds.add(Integer.valueOf(i));
			}
			// setting predicate to find by given branch Id
			subPredicate.or(qLoan.branchID.in(listBranchIds));
		}

		predicate.and(subPredicate);

		// find only enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));

		Iterable<Loan> iterable = loanRepository.findAll(predicate);
		Iterator<Loan> iterator = iterable.iterator();
		return iterator.hasNext() ? mapper.map(iterator.next(), LoanDTO.class) : null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanDetailsService#findRequiredDocument(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public List<SettingDocumentTypeDTO> findRequiredDocument(LoanDTO loanDTO) {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("findRequiredDocument by given product ID = {}", loanDTO.getProductId());
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getProductId())) {

			WorkFlowStepDTO parmsWorkFlowStepDTO = new WorkFlowStepDTO();
			parmsWorkFlowStepDTO.setProductId(loanDTO.getProductId());
			List<Long> listIdDocWFStep = new ArrayList<Long>();

			if (loanDTO.getEtapeWorkflow() > 23
					&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getStatutWorkflow())) {
				parmsWorkFlowStepDTO.setIdWorkFlowStep(loanDTO.getStatutWorkflow().longValue());
				List<WorkFlowStepDTO> listWorkFlowStepDTO =
						parametrageClient.findWorkFlowSteps(parmsWorkFlowStepDTO);
				if (!ACMValidationUtils.isNullOrEmpty(listWorkFlowStepDTO) && !ACMValidationUtils
						.isNullOrEmpty(listWorkFlowStepDTO.get(0).getDocuments())) {

					listWorkFlowStepDTO.get(0).getDocuments()
							.forEach(wfStepDoc -> listIdDocWFStep.add(wfStepDoc.getId()));

				}
			}
			List<SettingDocumentProductDTO> settingDocumentProductDTOs = parametrageClient
					.find(new SettingDocumentProductDTO(null, null, loanDTO.getProductId()));
			List<SettingDocumentTypeDTO> settingDocumentTypeDTOs = new ArrayList<>();
			// init list of ids
			settingDocumentProductDTOs.forEach(settingDocumentProductDTO -> {

				// get documents like in setting step
				if (loanDTO.getEtapeWorkflow() > 23
						&& !ACMValidationUtils.isNullOrEmpty(listIdDocWFStep)
						&& listIdDocWFStep.contains(settingDocumentProductDTO.getId())
						// TODO to be removed : Show only on Add Doc & USA Steps FOR OLD LOANS
						|| loanDTO.getEtapeWorkflow().equals(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey())
								&& settingDocumentProductDTO.getSettingDocumentTypeDTO()
										.getCategorie() < 2
						|| loanDTO.getEtapeWorkflow()
								.equals(CommonFunctions
										.mappingStatus(
												ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
										.getKey())
								&& settingDocumentProductDTO.getSettingDocumentTypeDTO()
										.getCategorie() == 2) {
					settingDocumentProductDTO.getSettingDocumentTypeDTO()
							.setMandatory(settingDocumentProductDTO.getMandatory());
					settingDocumentProductDTO.getSettingDocumentTypeDTO()
							.setReportName(settingDocumentProductDTO.getReportName());
					settingDocumentTypeDTOs
							.add(settingDocumentProductDTO.getSettingDocumentTypeDTO());
				}

			});
			return settingDocumentTypeDTOs;
		}
		return new ArrayList<>();
	}
}
