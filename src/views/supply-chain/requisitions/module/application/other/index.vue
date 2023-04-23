<template>
  <div class="aux-mat-requisitions-application-container">
    <common-wrapper :basic-class="currentBasicClass" :validate="validate" :show-total="false">
      <div class="filter-container">
        <div class="filter-right-box">
          <common-button class="filter-item" type="success" @click="materialSelectVisible = true"> 添加物料 </common-button>
          <excel-resolve-button
            icon="el-icon-upload2"
            btn-name="清单导入"
            btn-size="small"
            class="filter-item"
            btn-type="warning"
            open-loading
            :template="otherMaterialTemp"
            @success="handleExcelSuccess"
          />
        </div>
      </div>
      <el-form ref="formRef" :model="form">
        <aux-mat-table ref="tableRef" :max-height="tableMaxHeight" />
      </el-form>
    </common-wrapper>
    <common-drawer
      ref="drawerRef"
      v-model="materialSelectVisible"
      title="物料选择"
      :show-close="true"
      :size="900"
      custom-class="material-table-spec-select"
    >
      <template #content>
        <material-table-spec-select
          ref="matSpecRef"
          v-model="form.list"
          :visible="materialSelectVisible"
          :row-init-fn="rowInit"
          :max-height="specSelectMaxHeight"
          :basic-class="currentBasicClass"
          :table-width="430"
          auto-selected
          expand-query
        />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/requisitions-manage/requisitions'

import { defineProps, defineEmits, ref, provide, watch, nextTick } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { isBlank } from '@/utils/data-type'
import { preparationTypeEnum } from '@enum-ms/wms'
import otherMaterialTemp from '@/utils/excel/import-template/supply-chain/requisition-temp/other'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import commonWrapper from './../components/common-wrapper.vue'
import MaterialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import AuxMatTable from '../auxiliary-material/module/aux-mat-table.vue'
import excelResolveButton from '@/components-system/common/excel-resolve-button/index.vue'

import { ElMessage } from 'element-plus'

const emit = defineEmits(['success'])

const props = defineProps({
  detail: {
    type: Object
  }
})

const defaultForm = {
  list: [] // 入库清单
}

const tableRef = ref() // 表格ref
const matSpecRef = ref() // 规格列表ref
const formRef = ref() // form表单ref
const drawerRef = ref()

const materialSelectVisible = ref(false) // 显示物料选择
const currentBasicClass = matClsEnum.OTHER.V // 当前基础分类

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

const { cu, form, FORM } = useForm(
  {
    title: '其它申购',
    defaultForm: defaultForm,
    clearDraftCallback: init,
    api: crudApi.add
  },
  formRef,
  props.detail
)

watch(
  () => props.detail,
  (val = {}) => {
    form.type = val.type
    form.materialType = val.materialType
    // 项目id
    if (val.type === preparationTypeEnum.PROJECT.V) {
      form.projectId = val.projectId
    } else if (val.type === preparationTypeEnum.PUBLIC.V) {
      form.projectId = []
    } else {
      if (Array.isArray(val.projectId)) {
        if (val.projectId.length === 1) {
          form.projectId = val.projectId
        } else {
          form.projectId = []
        }
      } else {
        form.projectId = val.projectId ? [val.projectId] : []
      }
    }
  },
  { deep: true, immediate: true }
)

// 物料选择组件高度
const { maxHeight: specSelectMaxHeight } = useMaxHeight(
  {
    mainBox: '.material-table-spec-select',
    extraBox: '.el-drawer__header',
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

const { maxHeight: tableMaxHeight } = useMaxHeight({
  mainBox: '.requisitions-application-record-form',
  extraBox: [
    '.el-drawer__header',
    '.filter-container',
    '.requisitions-application-header',
    '.requisitions-application-select',
    '.requisitions-application-footer'
  ],
  wrapperBox: ['.el-drawer__body'],
  clientHRepMainH: true,
  navbar: false,
  minHeight: 300,
  extraHeight: 10
})

// 初始化
init()

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  emit('success')
  init()
}

// 表单校验
function validate() {
  if (form.type !== preparationTypeEnum.PUBLIC.V && isBlank(form.projectId)) {
    ElMessage.warning('请选择项目')
    return false
  }
  // 进入汇总页面
  return tableRef.value ? tableRef.value.validate() : true
}

// 行数据添加时初始化
function rowInit(row) {
  return tableRef.value.rowInit(row)
}

// 信息初始化
function init() {
  if (matSpecRef.value) {
    matSpecRef.value.clear()
  }
}

// 解析导入表格
function handleExcelSuccess(list) {
  // 解析
  // 根据物料种类获取
  try {
    cu.props.import(list)
  } catch (error) {
    ElMessage.error({ message: error.message, duration: 5000 })
  }
}

// 批量导入
cu.props.import = (importList) => {
  // 截取新旧数组长度，对导入数据进行rowWatch监听
  form.list.push.apply(form.list, importList)
  // 初始化选中数据，执行一次后取消当前监听
  const initSelectedTrigger = watch(
    matSpecRef,
    () => {
      if (matSpecRef.value) {
        matSpecRef.value.initSelected(
          importList.map((v) => {
            return { sn: v.sn, classifyId: v.classifyId }
          })
        )
        nextTick(() => {
          initSelectedTrigger()
        })
      }
    },
    { immediate: true }
  )
}
</script>

<style lang="scss" scoped>
.steel-inbound-application-container {
  position: relative;
  .header {
    padding: 20px 20px 10px 20px;
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
}
</style>
