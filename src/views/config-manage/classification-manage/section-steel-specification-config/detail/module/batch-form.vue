<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="crud.bStatus.title"
    :show-close="false"
    custom-class="section-steel-detail-batch-form"
    width="1000px"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
        <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">
          提 交
        </common-button>
        <store-operation type="crudBatch" />
        <common-button size="mini" @click="crud.cancelBCU">关 闭</common-button>
      </span>
    </template>
    <div>
      <el-form ref="formRef" :model="form" :disabled="crud.bStatus.cu === CRUD.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          empty-text="暂无数据"
          :max-height="maxHeight"
          default-expand-all
          :cell-class-name="wrongCellMask"
          style="width: 100%"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column key="name" prop="specification" :show-overflow-tooltip="true" label="规格">
            <template v-slot="scope">
              <el-input v-model.trim="scope.row.specification" type="text" clearable placeholder="规格" size="small" style="width: 100%" />
            </template>
          </el-table-column>
          <template v-for="sd in standard" :key="sd.id">
            <el-table-column :show-overflow-tooltip="true" :prop="`${prefix}${sd.id}`" :label="`${sd.name}\n理论重量(kg/m)`" align="center">
              <template v-slot="scope">
                <el-input-number
                  class="align-left"
                  v-model="scope.row[`${prefix}${sd.id}`]"
                  :placeholder="`${sd.name}`"
                  type="text"
                  controls-position="right"
                  style="width: 100%"
                  :min="0.001"
                />
              </template>
            </el-table-column>
          </template>
          <el-table-column label="操作" width="70px" align="center" fixed="right">
            <template v-slot="scope">
              <common-button
                type="danger"
                icon="el-icon-delete"
                size="mini"
                style="padding: 6px"
                @click.stop="removeRow(form.list, scope.$index)"
              />
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { inject, computed, ref } from 'vue'

import { regBatchForm } from '@compos/use-crud'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import StoreOperation from '@crud/STORE.operation.vue'

const tableRules = {
  specification: [{ required: true, max: 20, message: '不能超过50个字符', trigger: 'blur' }]
}

const prefix = inject('prefix')
const standard = inject('standard') // 国标
const sectionSteel = inject('sectionSteel') // 当前选择的型材

// 表单默认字段
const defaultForm = { list: [] }

// 行默认数据
const defaultRow = {
  specification: undefined
}

// 国标重量
standard.value.forEach((sd) => {
  defaultRow[`${prefix}${sd.id}`] = undefined
})

const formRef = ref()

const { CRUD, crud, form, ADD_FORM } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { init, addRow, removeRow } = useTableOperate(defaultRow, 10)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.section-steel-detail-batch-form',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

// 表格初始化
ADD_FORM.init = () => init(form.list)

// 表单校验
CRUD.HOOK.beforeValidateBCU = () => {
  const { validResult, dealList } = tableValidate(form.list)
  if (validResult) {
    form.list = dealList
  } else {
    return validResult
  }
}

// 表单提交数据清理
crud.submitBatchFormFormat = (form) => {
  // 型材id
  form.sectionSteelId = sectionSteel.value.id
  form.list.forEach((v) => {
    v.standard = []
    standard.value.forEach((sd) => {
      const unitNet = v[`${prefix}${sd.id}`]
      if (unitNet) {
        v.standard.push({
          id: sd.id, // 国标id
          unitNet: v[`${prefix}${sd.id}`] // 单位净量
        })
      }
      delete v[`${prefix}${sd.id}`]
    })
  })
  cleanUpData(form.list)
  return form
}
</script>
