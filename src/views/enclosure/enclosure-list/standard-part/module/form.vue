<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    :wrapper-closable="false"
    size="65%"
    custom-class="standard-part"
  >
    <template #titleAfter>
     <div>项目:<span>{{globalProject.serialNumber}}</span><span style="margin-left:5px;">{{globalProject.shortName}}</span></div>
    </template>
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight-110"
          style="width: 100%"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
           <el-table-column prop="useProperty" label="使用范围" align="center">
            <template v-slot="scope">
              <common-select
                v-model="scope.row.useProperty"
                :options="auxiliaryMaterialUseTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                placeholder="使用范围"
              />
            </template>
          </el-table-column>
          <el-table-column prop="name" label="名称" align="center" min-width="120">
            <template v-slot="scope">
              <el-input v-model.trim="scope.row.name" type="text" placeholder="名称" style="width:100%" maxlength="20"/>
            </template>
          </el-table-column>
          <el-table-column prop="specification" label="规格" align="center" min-width="150">
            <template v-slot="scope">
              <el-input v-model.trim="scope.row.specification" type="text" placeholder="规格" style="width:100%" maxlength="30"/>
            </template>
          </el-table-column>
          <el-table-column prop="measureUnit" label="单位" align="center">
            <template v-slot="scope">
              <el-input v-model.trim="scope.row.measureUnit" type="text" placeholder="单位" style="width:100%" maxlength="10"/>
            </template>
          </el-table-column>
          <el-table-column prop="quantity" label="数量" align="center">
            <template v-slot="scope">
              <common-input-number
                v-model="scope.row.quantity"
                :min="0"
                :max="999999999"
                :controls="false"
                :step="1"
                size="mini"
                placeholder="数量"
                @change="weightChange(scope.row)"
              />
            </template>
          </el-table-column>
          <el-table-column label="单重(kg)" prop="weight" align="center">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.weight"
                :min="0"
                :max="999999999"
                :step="1"
                :precision="DP.COM_WT__KG"
                :controls="false"
                placeholder="单重"
                style="width:100%;"
                @change="weightChange(scope.row)"
              />
            </template>
          </el-table-column>
          <el-table-column label="总重(kg)" prop="totalWeight">
            <template v-slot="scope">
              <span>{{toThousand(scope.row.totalWeight,DP.COM_WT__KG)}}</span>
            </template>
          </el-table-column>
          <el-table-column prop="remark" label="备注" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.remark"
                type="textarea"
                :autosize="{ minRows: 1, maxRows: 6 }"
                :maxlength="200"
                placeholder="备注"
                style="width:100%"/>
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" width="80">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button
            size="mini"
            icon="el-icon-circle-plus-outline"
            type="warning"
            style="margin-right: 15px"
            @click="addRow()"
            >继续添加</common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { inject, ref } from 'vue'
import { ElMessage } from 'element-plus'
import { regForm } from '@compos/use-crud'

import { auxiliaryMaterialUseTypeEnum } from '@enum-ms/plan'
import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'

const globalProject = inject('globalProject')

const formRef = ref()
const drawerRef = ref()

const defaultForm = {
  list: []
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.standard-part',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: false,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableRules = {
  useProperty: [{ required: true, message: '请输入选择使用范围', trigger: 'change' }],
  name: [{ required: true, message: '请输入名称', trigger: 'blur' }],
  specification: [{ required: true, message: '请输入规格', trigger: 'blur' }],
  measureUnit: [{ required: true, message: '请输入单位', trigger: 'blur' }],
  quantity: [{ required: true, message: '请输入数量', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    ...crud.query
  })
}

function weightChange(row) {
  row.totalWeight = (row.quantity && row.weight) ? row.quantity * row.weight : 0
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请先填写明细', type: 'error' })
    return false
  }
  const { validResult } = tableValidate(crud.form.list)
  if (!validResult) {
    return false
  }
}
</script>

<style lang="scss" scoped>
.add-row-box{text-align: center;margin-top:10px;}
</style>
