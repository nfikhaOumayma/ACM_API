/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import org.springframework.stereotype.Service;

import com.acm.service.ActivitiProcessService;

/**
 * {@link ActivitiProcessServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@Service
public class ActivitiProcessServiceImpl implements ActivitiProcessService {

	// /** The Constant logger. */
	// private static final Logger logger =
	// LoggerFactory.getLogger(ActivitiProcessServiceImpl.class);
	//
	// /** The activiti runtime service. */
	// @Autowired
	// private RuntimeService runtimeService;
	//
	// /** The task service default. */
	// @Autowired
	// private TaskService taskService;
	//
	// /*
	// * (non-Javadoc)
	// * @see com.acm.service.ActivitiProcessService#process(com.acm.utils.dtos.LoanDTO)
	// */
	// @Override
	// public LoanDTO process(LoanDTO loanDTO) {
	//
	// Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
	// logger.info(
	// "Start Processing Task for ProcessInstanceId = [{}] was updated for given loan with id = [{}]
	// and with ACCOUNT_NUMBER = {}",
	// loanDTO.getProcessInstanceId(), loanDTO.getLoanId(), loanDTO.getAccountNumber());
	// // process workflow for given loan by stored processInstanceId
	// String processInstanceId = loanDTO.getProcessInstanceId();
	// // find Task by given PROCESS_INSTANCE_ID
	// Task task =
	// taskService.createTaskQuery().processInstanceId(processInstanceId).singleResult();
	// if (task != null) {
	// Map<String, Object> taskVariables = new HashMap<>();
	// taskVariables.put(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
	// taskVariables.put(CommonConstants.TIMER_TASK_REMAINDER_KEY,
	// CommonConstants.TIMER_TASK_REMAINDER_VALUE);
	// taskService.complete(task.getId(), taskVariables);
	// logger.info(
	// "Task for ProcessInstanceId = [{}] was updated for given loan with id = [{}] and with
	// ACCOUNT_NUMBER = {}",
	// loanDTO.getProcessInstanceId(), loanDTO.getLoanId(),
	// loanDTO.getAccountNumber());
	// }
	// else {
	// logger.error(
	// "No Task was founded for ProcessInstanceId = [{}] for given loan with id = [{}] and with
	// ACCOUNT_NUMBER = {}",
	// loanDTO.getProcessInstanceId(), loanDTO.getLoanId(),
	// loanDTO.getAccountNumber());
	// }
	// return loanDTO;
	// }
	//
	// /*
	// * (non-Javadoc)
	// * @see com.acm.service.ActivitiProcessService#initProcess(com.acm.utils.dtos.LoanDTO,
	// * java.lang.String)
	// */
	// @Override
	// public ProcessInstance initProcess(LoanDTO loanDTO, String processName) {
	//
	// Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
	// Preconditions.checkNotNull(processName, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
	// logger.info(
	// "Starting workflow : {} for given LOAN with id = [{}] and with ACCOUNT_NUMBER = {}",
	// processName, loanDTO.getLoanId(), loanDTO.getAccountNumber());
	//
	// // start new workflow for given loan
	// Map<String, Object> variables = new HashMap<>();
	// variables.put(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
	// variables.put(CommonConstants.TIMER_TASK_REMAINDER_KEY,
	// CommonConstants.TIMER_TASK_REMAINDER_VALUE);
	// ProcessInstance processInstance =
	// runtimeService.startProcessInstanceByKey(processName, variables);
	//
	// logger.info("Process Instance ID : {} for Loan with ACCOUNT_NUMBER = {}",
	// processInstance.getId(), loanDTO.getAccountNumber());
	// return processInstance;
	// }
}
