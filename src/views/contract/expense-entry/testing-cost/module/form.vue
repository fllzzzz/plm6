<template>
  <common-dialog
    :title="showType === 'addBtn' ? '新增检测费' : '编辑检测费'"
    v-model="dialogVisible"
    width="500px"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button :loading="saveLoading" type="primary" size="mini" @click="save"> 提 交 </common-button>
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
          <upload-btn
            ref="uploadRef"
            v-model:files="form.attachmentFiles"
            :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
            :limit="1"
            :accept="'.pdf,.jpg,.jpeg,.png'"
          />
          <template v-if="form.attachments?.length > 0">
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
import { ref, inject, defineEmits, defineProps, watch } from 'vue'
import { add, edit } from '@/api/contract/expense-entry/testing-cost'
import { fileClassifyEnum } from '@enum-ms/file'
import { ElNotification } from 'element-plus'
import { deepClone } from '@data-type/index'
import UploadBtn from '@comp/file-upload/UploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'
import projectCascader from '@comp-base/project-cascader.vue'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'refresh', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  showType: {
    type: String
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

const form = ref({
  payDate: undefined,
  // projectId: undefined,
  testingFeeTypeId: undefined,
  feeAmount: undefined,
  attachmentIds: undefined,
  attachmentFiles: []
})
const formRef = ref()
const saveLoading = ref(false)
const dict = inject('dict')

const rules = {
  payDate: [{ required: true, message: '请选择支付时间', trigger: 'blur' }],
  testingFeeTypeId: [{ required: true, message: '请选择检测费类别', trigger: 'blur' }],
  feeAmount: [{ required: true, message: '请输入费用', trigger: 'blur' }]
  // projectId: [{ required: true, message: '请选择项目', trigger: 'blur' }]
}

function showHook() {
  if (props.showType === 'addBtn') {
    form.value = {}
  }
}

watch(
  () => props.visible,
  (val) => {
    if (props.showType !== 'addBtn') {
      form.value = Object.assign(form.value, props.info)
    }
  },
  { immediate: true }
)

async function save() {
  try {
    saveLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return false
    const _form = deepClone(form.value)
    _form.attachmentIds = _form.attachmentIds ? [Number(_form.attachmentIds)] : _form.attachmentFiles?.map((v) => v.id)
    _form.attachments = _form.attachmentFiles
    props.showType === 'addBtn' ? await add(_form) : await edit(_form)
    ElNotification({ title: props.showType === 'addBtn' ? '新增成功' : '编辑成功', type: 'success' })
    handleClose()
    emit('success')
    emit('refresh')
  } catch (error) {
    console.log('新增失败', error)
  } finally {
    saveLoading.value = false
  }
}
function disabledDate(time) {
  return time > new Date()
}
</script>

<style lang="scss" scoped></style>
