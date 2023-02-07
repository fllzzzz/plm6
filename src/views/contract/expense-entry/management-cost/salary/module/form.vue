<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    width="25%"
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
        <el-form-item label="月份：" prop="month">
          <el-input-number v-model="form.month" style="width: 270px" placeholder="输入月份" controls-position="right" :min="1" :max="12" />
        </el-form-item>
        <el-form-item label="员工人数：" prop="employeeQuantity">
          <el-input-number
            v-show-thousand
            v-model="form.employeeQuantity"
            style="width: 270px"
            placeholder="输入员工人数"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item label="工资总额（元）：" prop="totalWage">
          <el-input-number
            v-show-thousand
            v-model="form.totalWage"
            style="width: 270px"
            placeholder="输入工资总额"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item label="附件：" prop="attachments">
          <upload-btn ref="uploadRef" v-model:files="form.attachmentFiles" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1"  :accept="'.pdf,.jpg,.jpeg,.png'"/>
          <template v-if="form.attachments?.length > 0 && (!form.attachmentFiles || form.attachmentFiles.length===0)">
            <div v-for="item in form.attachments" :key="item.id">
              {{ item.name }}
              <export-button :params="{ id: item.id }" />
            </div>
          </template>
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import { fileClassifyEnum } from '@enum-ms/file'
import UploadBtn from '@comp/file-upload/UploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'

const prop = defineProps({
  query: {
    type: Object
  }
})

const formRef = ref()

const defaultForm = {
  id: undefined,
  month: undefined,
  employeeQuantity: undefined,
  attachmentFiles: [],
  totalWage: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  month: [{ required: true, message: '请输入月份', trigger: 'blur' }],
  employeeQuantity: [{ required: true, message: '请输入员工总额', trigger: 'blur' }],
  totalWage: [{ required: true, message: '请输入工资总额', trigger: 'blur' }]
}

// 新增之前
CRUD.HOOK.beforeToAdd = (crud, form) => {
  crud.form.attachmentIds = undefined
}

// 处理刷新数据
CRUD.HOOK.beforeToQuery = async () => {}
// 编辑之前
CRUD.HOOK.beforeToEdit = () => {
  crud.form.attachmentIds = crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  form.wageType = prop.query.wageType
  form.year = prop.query.year
  crud.form.attachmentIds = crud.form.attachmentFiles ? crud.form.attachmentFiles.map((v) => v.id) : crud.form.attachmentIds
}
</script>

<style lang="scss" scoped></style>
