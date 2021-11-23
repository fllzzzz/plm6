<template>
<common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="`${crud.props.factory ? crud.props.factory.name + '：' : ''}${crud.bStatus.title}`"
    :show-close="false"
    custom-class="warehouse-batch-add"
    width="1000px"
    top="10vh"
  >
    <template #titleRight>
      <span style="float:right">
        <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
        <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">提 交</common-button>
        <store-opertaion type="crudBatch" />
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
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="仓库位置" width="220">
          <template v-slot="scope">
            <el-input
              v-model.trim="scope.row.name"
              type="text"
              clearable
              placeholder="仓库位置"
              size="small"
              style="width: 100%;"
            />
          </template>
        </el-table-column>
        <el-table-column key="materialType" prop="materialType" :show-overflow-tooltip="true" label="可存储材料类型" min-width="160">
          <template v-slot="scope">
            <common-select
              v-model="scope.row.materialType"
              :options="materialClassificationEnum.ENUM"
              multiple
              clearable
              type="enum"
              placeholder="可存储材料类型"
              style="width: 100%;"
            />
          </template>
        </el-table-column>
        <el-table-column key="type" prop="type" :show-overflow-tooltip="true" label="仓库类型" width="125">
          <template v-slot="scope">
            <common-select
              v-model="scope.row.type"
              :options="warehouseTypeEnum.ENUM"
              show-extra
              :extra-val="dittos.get('type')"
              clearable
              type="enum"
              placeholder="仓库类型"
              style="width: 100%;"
            />
          </template>
        </el-table-column>
        <el-table-column key="sort" prop="sort" :show-overflow-tooltip="true" label="排序" align="center" width="100">
          <template v-slot="scope">
            <el-input-number
              v-model="scope.row.sort"
              type="text"
              maxlength="3"
              size="small"
              placeholder="排序"
              style="width: 100%;"
              :controls="false"
            />
          </template>
        </el-table-column>
        <el-table-column
          label="操作"
          width="50"
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
// TODO: 考虑根据工厂id存为草稿
import { computed, ref } from 'vue'
import EO from '@enum'
import { warehouseTypeEnum } from '@enum-ms/wms'
import { materialClassificationEnum } from '@enum-ms/classification'

import { regBatchForm } from '@compos/use-crud'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import StoreOpertaion from '@crud/STORE.opertaion.vue'

const tableRules = {
  name: [{ required: true, max: 20, message: '不能超过20个字符', trigger: 'blur' }],
  materialType: [{ required: true, message: '请选择仓库存储的材料类型', trigger: 'change' }],
  type: [{ required: true, message: '请选择仓库类型', trigger: 'change' }],
  sort: [{ max: 3, message: '不能超过3个字符', trigger: 'blur' }]
}

const defaultForm = { list: [] }

const defaultRow = {
  name: undefined,
  materialType: undefined,
  type: warehouseTypeEnum.NORMAL.V,
  sort: undefined
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
    mainBox: '.warehouse-batch-add',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

// 表格初始化
ADD_FORM.init = () => {
  init(form.list)
}
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
  cleanUpData(form.list)
  form.list.forEach(v => {
    v.materialType = EO.getBitsSum(v.materialType)
  })
  form.factoryId = crud.query.factoryId
  return form
}
</script>