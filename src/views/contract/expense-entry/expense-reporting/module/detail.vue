<template>
  <common-drawer
    :title="`费用填报审批${unreviewed ? '' : '详情'}`"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    size="40%"
  >
    <template #titleRight>
      <template v-if="unreviewed">
        <common-button
          v-permission="permission.audit"
          v-loading="submitLoading"
          size="mini"
          type="success"
          @click="submit(reviewStatusEnum.PASS.V)"
        >
          通过
        </common-button>
        <common-button
          v-permission="permission.audit"
          v-loading="submitLoading"
          size="mini"
          type="danger"
          @click="submit(reviewStatusEnum.REFUSE.V)"
        >
          拒绝
        </common-button>
      </template>
    </template>
    <template #content>
      <el-form label-width="80px" size="mini">
        <el-row :gutter="0">
          <el-col :span="24">
            <el-form-item label="项目" prop="project">
              <div>{{ props.detail.project }}</div>
            </el-form-item>
          </el-col>

          <el-col :span="12">
            <el-form-item label="部门" prop="deptName">
              <div>{{ props.detail.deptName }}</div>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="填报人" prop="writtenByName">
              <div>{{ props.detail.writtenByName }}</div>
            </el-form-item>
          </el-col>

          <el-col :span="12">
            <el-form-item label="报销人" prop="reimburseUserName">
              <div>{{ props.detail.reimburseUserName }}</div>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="费用类别" prop="expenseTypeName">
              <div>{{ props.detail.expenseTypeName }}</div>
            </el-form-item>
          </el-col>

          <el-col :span="12">
            <el-form-item label="报销日期" prop="reimburseDate">
              <div>{{ props.detail.reimburseDate }}</div>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="费用科目" prop="expenseSubjectName">
              <div>{{ props.detail.expenseSubjectName }}</div>
            </el-form-item>
          </el-col>

          <el-col :span="12">
            <el-form-item label="报销金额" prop="reimburseAmount">
              <div>{{ props.detail.reimburseAmount }}</div>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="费用归属" prop="costAscriptionEnum">
              <div>{{ props.detail.costAscriptionEnum }}</div>
            </el-form-item>
          </el-col>

          <el-col :span="24">
            <el-form-item label="收款单位" prop="payee">
              <div>{{ props.detail.payee }}</div>
            </el-form-item>
          </el-col>

          <el-col :span="24">
            <el-form-item label="备注" prop="remark">
              <div>{{ props.detail.remark }}</div>
            </el-form-item>
          </el-col>

          <el-col :span="24">
            <upload-list
              v-if="checkPermission(permission.audit) && unreviewed"
              :file-classify="fileClassifyEnum.EXPENSE_REIMBURSE_APPROVAL.V"
              v-model:files="attachments"
              show-download
              empty-text="暂未上传费用填报审批附件"
              style="margin-bottom: 20px"
            />
            <upload-list
              v-else
              :file-classify="fileClassifyEnum.EXPENSE_REIMBURSE_APPROVAL.V"
              v-model:files="attachments"
              show-download
              :uploadable="false"
              empty-text="暂未上传费用填报审批附件"
              style="margin-bottom: 20px"
            />
          </el-col>

          <el-col :span="24">
            <el-input
              v-if="checkPermission(permission.audit) && unreviewed"
              v-model.trim="approvalRemark"
              type="textarea"
              :autosize="{ minRows: 3, maxRows: 8 }"
              placeholder="请填写审批备注"
              :maxlength="300"
              show-word-limit
            />
            <el-form-item v-else label="审批备注" prop="approvalRemark">
              <div>{{ approvalRemark }}</div>
            </el-form-item>
          </el-col>
        </el-row>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { audit } from '@/api/contract/expense-entry/expense-reporting'
import { defineProps, defineEmits, ref, watch, inject, computed } from 'vue'
import { ElNotification } from 'element-plus'

import { fileClassifyEnum } from '@enum-ms/file'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import checkPermission from '@/utils/system/check-permission'

import useVisible from '@compos/use-visible'
import uploadList from '@comp/file-upload/UploadList.vue'

const permission = inject('permission')

const submitLoading = ref(false)
const attachments = ref([])
const approvalRemark = ref('')

const emit = defineEmits(['success', 'update:modelValue'])
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detail: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })

const unreviewed = computed(() => {
  return props.detail?.sourceRow?.approvalStatus === reviewStatusEnum.UNREVIEWED.V
})

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      attachments.value = props.detail.attachmentList
      approvalRemark.value = props.detail.approvalRemark
    }
  }
)

async function submit(approvalStatus) {
  try {
    submitLoading.value = true
    const data = {
      approvalStatus,
      id: props.detail.id,
      approvalRemark: approvalRemark.value,
      attachmentIds: attachments.value.map((row) => row.id)
    }
    await audit(data)
    ElNotification({ title: '审核成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('费用填报审核', error)
  } finally {
    submitLoading.value = false
  }
}
</script>

