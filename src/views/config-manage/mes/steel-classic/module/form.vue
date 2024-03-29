<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    ref="drawerRef"
    size="60%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <div class="detail-header">
          <el-form-item label="名称" prop="name">
          <el-input v-model="form.name" type="text" placeholder="分类名称" style="width: 270px" maxlength="30" />
          </el-form-item>
          <el-form-item label="零件科目匹配" prop="classifyIds">
            <common-radio-button
              v-model="form.basicClass"
              :options="[matClsEnum.STEEL_PLATE]"
              type="enum"
              size="small"
              style="margin-bottom:5px;"
            />
            <br/>
            <material-cascader
              v-model="form.classifyIds"
              :basic-class="form.basicClass"
              :disabled="!form.basicClass"
              multiple
              :collapse-tags="false"
              separator=" > "
              clearable
              :disabledVal="disabledClassifyIds"
              placeholder="请选择科目"
              size="small"
              style="width: 270px"
            />
          </el-form-item>
          <el-form-item label="排序" prop="sort">
            <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
          </el-form-item>
          <el-divider><span class="title">零件规格前缀明细</span></el-divider>
        </div>
         <common-table
          ref="detailRef"
          border
          :data="form.links"
          :max-height="maxHeight"
          style="width: 100%;margin-top:10px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="keyword" prop="keyword" label="*大写字母" align="center">
            <template v-slot="scope">
              <common-select
                v-if="form.basicClass === matClsEnum.STEEL_PLATE.V"
                v-model="scope.row.keyword"
                :options="[partKeyWordEnum.PL]"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="大写字母"
                @change="checkName(scope.row, scope.$index)"
              />
              <el-input v-else v-model.trim="scope.row.keyword" type="text" placeholder="大写字母" maxlength="20" @blur="checkName(scope.row, scope.$index)"/>
            </template>
          </el-table-column>
          <el-table-column key="specIndex" prop="specIndex" label="*索引" align="center">
            <template v-slot="scope">
              <common-select
                v-model="scope.row.specIndex"
                :options="specIndexEnum"
                showOptionAll
                :allVal="0"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="索引"
              />
            </template>
          </el-table-column>
          <!-- <el-table-column key="boolNestEnum" prop="boolNestEnum" label="*是否参与套料" align="center">
            <template v-slot="scope">
              <common-radio v-model="scope.row.boolNestEnum" :options="whetherEnum.ENUM" type="enum" />
            </template>
          </el-table-column>
          <el-table-column key="boolSchedulingEnum" prop="boolSchedulingEnum" label="*是否排产" align="center">
            <template v-slot="scope">
              <common-radio v-model="scope.row.boolSchedulingEnum" :options="whetherEnum.ENUM" type="enum" />
            </template>
          </el-table-column> -->
          <el-table-column label="操作" align="center">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="delProcess(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div style="text-align:center;margin-top:10px;">
          <common-button size="mini" type="success" @click="addProcess">添加</common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, nextTick, watch } from 'vue'
import { ElMessage } from 'element-plus'

import { isNotBlank, deepClone } from '@data-type/index'
import { matClsEnum } from '@enum-ms/classification'
import { partKeyWordEnum } from '@enum-ms/mes'

import { regForm } from '@compos/use-crud'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'

const props = defineProps({
  boundAllClassifyIds: {
    type: Array,
    default: () => {}
  }
})

const formRef = ref()
const nameArr = ref([])
const drawerRef = ref()
const disabledClassifyIds = ref([])
const specIndexEnum = {
  1: { L: '1', K: '1', V: 1 },
  2: { L: '2', K: '2', V: 2 },
  3: { L: '3', K: '3', V: 3 },
  4: { L: '4', K: '4', V: 4 }
}
const defaultForm = {
  id: undefined,
  name: '',
  basicClass: matClsEnum.STEEL_PLATE.V,
  sort: undefined,
  links: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.detail-header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    extraHeight: 120
  },
  () => drawerRef.value.loaded
)

// 序号校验
const validateEnum = (value, row) => {
  if (!isNotBlank(value)) return false
  return true
}

const tableRules = {
  keyword: [{ required: true, message: '请输入大写字母', trigger: 'blur' }],
  specIndex: [{ validator: validateEnum, message: '请选择索引', trigger: 'blur' }]
  // boolNestEnum: [{ validator: validateEnum, message: '请选择是否参与套料', trigger: 'change' }],
  // boolSchedulingEnum: [{ validator: validateEnum, message: '请选择是否排产', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

const rules = {
  name: [
    { required: true, message: '请填写分类名称', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  classifyIds: [{ required: true, message: '请选择科目', trigger: 'change' }]
}

watch(
  () => form.basicClass,
  (val, oldVal) => {
    if (oldVal === matClsEnum.STEEL_PLATE.V) {
      nameArr.value = []
      form.links = []
    }
    if (val === matClsEnum.STEEL_PLATE.V && oldVal && oldVal !== matClsEnum.STEEL_PLATE.V) {
      nameArr.value = []
      form.links = []
    }
    if (val && oldVal) {
      form.classifyIds = []
    }
  }
)

function addProcess() {
  form.links.push({
    add: true
  })
}
function delProcess(index) {
  form.links.splice(index, 1)
}

function checkName(item, index) {
  const val = nameArr.value.find((v) => v.index === index)
  if (val) {
    if (item.keyword) {
      if (val.keyword === item.keyword) {
        return
      }
      if (nameArr.value.findIndex((v) => v.keyword === item.keyword) > -1) {
        ElMessage({
          message: '关键字母已存在，请重新填写',
          type: 'error'
        })
        nextTick(() => {
          form.links[index].keyword = undefined
          val.keyword = undefined
        })
      } else {
        if (form.basicClass !== matClsEnum.STEEL_PLATE.V && item.keyword.substring(0, 1) === 'P') {
          ElMessage({
            message: '关键字母不能以P或PL开头，请重新填写',
            type: 'error'
          })
          nextTick(() => {
            form.links[index].keyword = undefined
            val.keyword = undefined
          })
        } else {
          nextTick(() => {
            val.keyword = item.keyword
          })
        }
      }
    } else {
      nextTick(() => {
        val.keyword = undefined
      })
    }
  } else {
    if (item.keyword) {
      if (nameArr.value.findIndex((v) => v.keyword === item.keyword) > -1) {
        ElMessage({
          message: '关键字母已存在，请重新填写',
          type: 'error'
        })
        nextTick(() => {
          item.keyword = undefined
        })
      }
      if (form.basicClass !== matClsEnum.STEEL_PLATE.V && item.keyword.substring(0, 1) === 'P') {
        ElMessage({
          message: '关键字母不能以P或PL开头，请重新填写',
          type: 'error'
        })
        nextTick(() => {
          item.keyword = undefined
        })
      }
      if (item.keyword) {
        nameArr.value.push({
          keyword: item.keyword,
          index: index
        })
      }
    }
  }
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  if (crud.form.links && crud.form.links.length === 0) {
    ElMessage.error('请填写零件规格前缀明细')
    return false
  }
  const { validResult, dealList } = tableValidate(crud.form.links)
  if (validResult) {
    crud.form.links = dealList
  } else {
    return validResult
  }
}

CRUD.HOOK.beforeToAdd = () => {
  nameArr.value = []
}

CRUD.HOOK.beforeToEdit = () => {
  nameArr.value = []
  form.links.map((v, index) => {
    nameArr.value.push({
      keyword: v.keyword,
      index: index
    })
  })
}

CRUD.HOOK.beforeToCU = () => {
  nextTick(() => {
    disabledClassifyIds.value = deepClone(props.boundAllClassifyIds)
    form.classifyIds &&
      form.classifyIds.forEach((v) => {
        const _index = disabledClassifyIds.value.indexOf(v)
        if (_index !== -1) {
          disabledClassifyIds.value.splice(_index, 1)
        }
      })
  })
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;
  .process-box {
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
    align-items: flex-start;
    .process-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-start;
      align-items: center;
      margin-bottom: 10px;
    }
  }
}
</style>
