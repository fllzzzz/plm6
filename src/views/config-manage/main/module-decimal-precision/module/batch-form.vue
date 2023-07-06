<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="crud.bStatus.title"
    :show-close="false"
    custom-class="decimal-batch-add"
    width="700px"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
        <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">
          提 交
        </common-button>
        <!-- <store-operation type="crudBatch" /> -->
        <common-button size="mini" @click="crud.cancelBCU">关 闭</common-button>
      </span>
    </template>
    <div>
      <el-form ref="formRef" :model="form" :disabled="crud.bStatus.cu === CRUD.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          :show-empty-symbol="false"
          return-source-data
          empty-text="暂无数据"
          :max-height="maxHeight"
          default-expand-all
          :cell-class-name="wrongCellMask"
          style="width: 100%"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column key="menuId" prop="menuId" :show-overflow-tooltip="true" label="模块">
            <template #default="{ row }">
              <common-select
                v-model="row.menuId"
                :options="props.menuArr"
                type="other"
                :data-structure="{ key: 'id', label: 'name', value: 'id' }"
                :disabled-val="menuDisabledId"
                placeholder="模块"
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column key="type" prop="type" label="小数类型" width="100px">
            <template #default>
              <span>金额</span>
            </template>
          </el-table-column>
          <el-table-column key="scale" prop="scale" label="小数精度" align="center">
            <template #default="{ row }">
              <el-input-number
                v-model.number="row.scale"
                :min="1"
                :max="10"
                :precision="0"
                :step="1"
                placeholder="小数精度"
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" width="70px" align="center" fixed="right">
            <template #default="{ $index }">
              <common-button
                type="danger"
                icon="el-icon-delete"
                size="mini"
                style="padding: 6px"
                @click.stop="removeRow(form.list, $index)"
              />
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { computed, ref, defineProps } from 'vue'

import { regBatchForm } from '@compos/use-crud'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  menuArr: {
    type: Array,
    default: () => []
  },
  disabledId: {
    type: Array,
    default: () => []
  }
})

const menuDisabledId = computed(() => {
  const ids = form.list?.map(v => v.menuId) || []
  return [...ids, ...props.disabledId]
})
const tableRules = {
  menuId: [{ required: true, message: '请选择模块', trigger: 'change' }],
  scale: [{ required: true, message: '请输入小数精度', trigger: 'change' }]
}

const defaultForm = { list: [] }

const defaultRow = {
  menuId: undefined,
  scale: undefined
}

// 同上的选项与值
const ditto = new Map([])

const formRef = ref()

const { CRUD, crud, form, ADD_FORM } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { init, addRow, removeRow } = useTableOperate(defaultRow, 10, ditto)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.decimal-batch-add',
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
