<template>
   <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    :title="showType==='audit'?'分包签证审核':'分包签证详情'"
    :wrapper-closable="false"
    size="800px"
  >
    <template #titleAfter>
      <el-tag v-if="currentRow.auditStatus" size="medium" :type="currentRow.auditStatus===auditTypeEnum.REJECT.V?'info':(currentRow.auditStatus===auditTypeEnum.PASS.V?'success':'warning')">
        {{ currentRow.auditStatus===auditTypeEnum.REJECT.V?'已驳回':(currentRow.auditStatus===auditTypeEnum.PASS.V?'已通过':'审核中') }}
      </el-tag>
    </template>
    <template #titleRight>
      <export-button
        v-if="checkPermission(permission.download)"
        :params="currentRow.id"
        :fn="download"
      >下载签证单</export-button>
      <common-button v-loading="submitLoading" v-if="showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
      <common-button v-loading="submitLoading" v-if="showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="130px">
        <el-form-item label="所属项目">
          <span>{{ currentRow?.project }}</span>
        </el-form-item>
        <div style="display:flex;">
          <el-form-item label="签证单位" style="width:50%;">
            <span>{{ currentRow?.supplierName }}</span>
          </el-form-item>
          <el-form-item label="分类" style="width:50%;">
            <span>{{ currentRow?.subcontractClassName }}</span>
          </el-form-item>
        </div>
         <div style="display:flex;">
          <el-form-item label="申请日期" style="width:50%;">
            <span>{{ currentRow?.applyDate }}</span>
          </el-form-item>
          <el-form-item label="签证原因" style="width:50%;">
            <span>{{ currentRow?.visaReasonName }}</span>
          </el-form-item>
        </div>
        <el-form-item label="申请签证额">
          <span>{{ currentRow?.visaAmount }}</span>
        </el-form-item>
        <el-form-item label="描述">
          <span>{{ currentRow?.remark }}</span>
        </el-form-item>
        <el-form-item label="图片">
          <div class="imgs-box">
            <el-image
              v-for="url in currentRow.attachments"
              :preview-src-list="currentRow.imgSrc"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              lazy
              style="margin:0 2px;"
            ></el-image>
          </div>
        </el-form-item>
        <div style="display:flex;">
          <el-form-item label="审批人" style="width:50%;">
            <user-dept-cascader
              v-if="showType==='audit'"
              v-model="approverId"
              filterable
              :collapse-tags="false"
              clearable
              show-all-levels
              style="width: 220px"
              placeholder="审批人"
            />
            <span v-else>{{ currentRow?.approverName }}</span>
          </el-form-item>
          <el-form-item label="日期" style="width:50%;">
            <el-date-picker
              v-if="showType==='audit'"
              v-model="approveTime"
              type="date"
              value-format="x"
              placeholder="审批日期"
              style="width: 260px"
              :disabledDate="(date) => { return date.getTime() > new Date().getTime() }"
            />
            <span v-else>{{ currentRow?.approveTime }}</span>
          </el-form-item>
        </div>
        <div style="display:flex;" v-if="showType === 'detail'">
          <el-form-item label="审核人" style="width:50%;">
            <span>{{ currentRow?.auditUserName }}</span>
          </el-form-item>
          <el-form-item label="日期" style="width:50%;">
            <span>{{ currentRow?.auditTime }}</span>
          </el-form-item>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits } from 'vue'
import { visaAudit, download } from '@/api/project-manage/subcontract-visa-manage'
import { ElNotification } from 'element-plus'

import useVisible from '@compos/use-visible'
import { auditTypeEnum } from '@enum-ms/contract'
import checkPermission from '@/utils/system/check-permission'

import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: 'detail'
  }
})

const submitLoading = ref(false)
const approverId = ref()
const approveTime = ref()

async function passConfirm(val) {
  submitLoading.value = true
  const msg = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
  try {
    const submitForm = {
      auditStatus: val,
      id: props.currentRow.id,
      approverId: approverId.value,
      approveTime: approveTime.value
    }
    await visaAudit(submitForm)
    submitLoading.value = false
    ElNotification({ title: '审核' + msg, type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    submitLoading.value = false
    console.log('分包签证审核失败', error)
  }
}

</script>
<style lang="scss" scoped>
.imgs-box {
  & > .el-image {
    width: 50px;
    height: 40px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
