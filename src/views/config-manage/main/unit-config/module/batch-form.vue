<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="crud.bStatus.title"
    :show-close="false"
    custom-class="unit-batch-add"
    width="700px"
    top="10vh"
  >
    <template #titleRight>
      <span style="float:right">
        <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
        <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">提 交</common-button>
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
        style="width: 100%;"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="名称" min-width="150">
          <template v-slot="scope">
            <el-input
              v-model.trim="scope.row.name"
              type="text"
              clearable
              placeholder="名称"
              size="small"
              style="width: 100%;"
            />
          </template>
        </el-table-column>
        <el-table-column key="symbol" prop="symbol" :show-overflow-tooltip="true" label="符号" width="125">
          <template v-slot="scope">
            <el-input
              v-model.trim="scope.row.symbol"
              type="text"
              maxlength="3"
              size="small"
              placeholder="符号"
              style="width: 100%;"
            />
          </template>
        </el-table-column>
        <el-table-column key="type" prop="type" label="类型" width="192">
          <template v-slot="scope">
            <common-select
              v-model="scope.row.type"
              :options="unitTypeEnum.ENUM"
              show-extra
              :extra-val="dittos.get('type')"
              type="enum"
              placeholder="类型"
              style="width: 170px;"
            />
          </template>
        </el-table-column>
        <el-table-column
          label="操作"
          width="70px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <common-button type="danger" icon="el-icon-delete" size="mini" style="padding:6px" @click.stop="removeRow(form.list, scope.$index)" />
          </template>
        </el-table-column>
      </common-table>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { computed, ref } from 'vue'
import { unitTypeEnum } from '@enum-ms/common'

import { regBatchForm } from '@compos/use-crud'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import StoreOperation from '@crud/STORE.operation.vue'

const tableRules = {
  name: [{ required: true, max: 20, message: '不能超过20个字符', trigger: 'blur' }],
  symbol: [{ max: 3, message: '不能超过3个字符', trigger: 'blur' }],
  type: [{ required: true, message: '请选择单位类型', trigger: 'change' }]
}

const defaultForm = { list: [] }

const defaultRow = {
  name: undefined,
  symbol: undefined,
  type: unitTypeEnum.DIGIT.V
}

// 同上的选项与值
const dittos = new Map([
  ['type', -1]
])

const formRef = ref()

const { CRUD, crud, form, ADD_FORM } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { init, addRow, removeRow } = useTableOperate(defaultRow, 10, dittos)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, dittos })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.unit-batch-add',
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
crud.submitBatchFormFormat = (form) => cleanUpData(form.list)
</script>
