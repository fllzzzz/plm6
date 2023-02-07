<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    width="30%"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
          提 交
        </common-button>
        <common-button size="mini" @click="crud.cancelCU">关 闭</common-button>
      </span>
    </template>
    <div class="form">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" class="demo-form">
        <el-form-item label="支付时间：" prop="payDate">
          <el-date-picker
            v-model="form.payDate"
            type="date"
            size="small"
            class="date-item filter-item"
            placeholder="选择日期"
            format="YYYY-MM-DD"
            value-format="x"
            :disabled-date="disabledDate"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="项目：" prop="projectId">
          <project-cascader v-model="form.projectId" clearable class="filter-item" style="width: 270px" />
        </el-form-item>
        <el-form-item label="检测费类别：" prop="testingFeeTypeId">
          <common-select
            v-model="form.testingFeeTypeId"
            :options="dict.testing_fee_type"
            type="other"
            :data-structure="{ key: 'id', label: 'label', value: 'id' }"
            class="filter-item"
            clearable
            style="width: 270px"
            placeholder="选择检测费类别"
          />
        </el-form-item>
        <el-form-item label="费用：" prop="feeAmount">
          <el-input ref="saveTagInput" v-model="form.feeAmount" placeholder="输入费用 单位（元）" style="width: 270px" />
        </el-form-item>
        <el-form-item label="附件：" prop="attachments">
          <upload-btn ref="uploadRef" v-model:files="form.attachmentFiles" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1"  :accept="'.pdf,.jpg,.jpeg,.png'" />
          <template v-if="form.attachments?.length > 0 && (!form.attachmentFiles || form.attachmentFiles.length===0)">
            <div v-for="item in form.attachments" :key="item.id">
              {{ item.name }}
              <export-button :params="{ id: item.id }" />
            </div>
          </template>
        </el-form-item>
        <el-form-item label="备注：" prop="remark">
          <el-input ref="saveTagInput" v-model="form.remark" placeholder="输入备注" style="width: 270px" />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, inject } from 'vue'
import { regForm } from '@compos/use-crud'
import { fileClassifyEnum } from '@enum-ms/file'
import UploadBtn from '@comp/file-upload/UploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'
import projectCascader from '@comp-base/project-cascader.vue'

const formRef = ref()
const dict = inject('dict')
const defaultForm = {
  payDate: undefined,
  projectId: undefined,
  testingFeeTypeId: undefined,
  feeAmount: undefined,
  attachmentIds: undefined,
  attachmentFiles: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  payDate: [{ required: true, message: '请选择支付时间', trigger: 'blur' }],
  testingFeeTypeId: [{ required: true, message: '请选择检测费类别', trigger: 'blur' }],
  feeAmount: [{ required: true, message: '请输入费用', trigger: 'blur' }],
  projectId: [{ required: true, message: '请选择项目', trigger: 'blur' }]
}

// 处理刷新数据
CRUD.HOOK.beforeToQuery = async () => {}

// 新增之前
CRUD.HOOK.beforeToAdd = () => {
  crud.form.attachmentIds = undefined
}

// 编辑之前
CRUD.HOOK.beforeToEdit = () => {
  form.projectId = form.project?.id
  crud.form.attachmentIds = crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined
}

// 编辑之后
CRUD.HOOK.afterToEdit = (crud, form) => {}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  crud.form.attachmentIds = crud.form.attachmentFiles ? crud.form.attachmentFiles.map((v) => v.id) : crud.form.attachmentIds
}

function disabledDate(time) {
  return time > new Date()
}
</script>

<style lang="scss" scoped></style>
