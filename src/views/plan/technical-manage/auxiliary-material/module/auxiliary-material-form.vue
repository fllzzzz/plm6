<template>
  <div class="aux-mat-inbound-application-container">
    <common-wrapper
      :basic-class="currentBasicClass"
      :validate="validate"
      :edit="props.edit"
      :show-total="false"
    >
      <div class="filter-container">
        <div class="filter-left-box">
          <common-button class="filter-item" type="success" @click="materialSelectVisible = true">
            添加物料
          </common-button>
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
// TODO: 编辑，反向赋值
import { add as saveMaterial } from '@/api/plan/technical-manage/plan-auxiliary-material'
import { defineProps, defineEmits, ref, watch, provide, nextTick, reactive, inject } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import CommonWrapper from './material/common-wrapper.vue'
import MaterialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import AuxMatTable from './material/aux-mat-table.vue'

const emit = defineEmits(['success'])

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  detail: {
    type: Object
  }
})

const defaultForm = {
  projectId: null, // 项目id
  monomerId: null, // 单体id
  list: [] // 清单
}

const tableRef = ref() // 表格ref
const matSpecRef = ref() // 规格列表ref
const formRef = ref() // form表单ref
const drawerRef = ref()

const materialSelectVisible = ref(false) // 显示物料选择
const currentBasicClass = matClsEnum.MATERIAL.V // 当前基础分类

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

const currentMonomer = inject('currentMonomer')
const globalProject = inject('globalProject')

// 使用草稿/修改时，为数据设置监听
const setFormCallback = (form) => {
  form.list = form.list.map((v) => reactive(v))
  const trigger = watch(
    tableRef,
    (ref) => {
      if (ref) {
        // 初始化选中数据，执行一次后取消当前监听
        const initSelectedTrigger = watch(
          matSpecRef,
          () => {
            if (matSpecRef.value) {
              matSpecRef.value.initSelected(
                form.list.map((v) => {
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
        nextTick(() => {
          trigger()
        })
      }
    },
    { immediate: true, deep: true }
  )
  fixMaxHeight()
}

const { form, FORM } = useForm(
  {
    title: '配套件清单',
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    api: saveMaterial
  },
  formRef,
  props.detail
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

const tableHeightConfig = {
  mainBox: '.aux-mat-inbound-application-container',
  extraBox: ['.filter-container', '.inbound-application-header', '.inbound-application-footer'],
  navbar: true,
  minHeight: 300,
  extraHeight: 20
}

const { fixMaxHeight, maxHeight: tableMaxHeight } = useMaxHeight(tableHeightConfig)
// 初始化
init()

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  emit('success')
  init()
}

FORM.HOOK.beforeSubmit = () => {
  form.projectId = globalProject.value.id
  form.monomerId = currentMonomer.value.id
}

// 表单校验
function validate() {
  // 提交预览页面
  return tableRef.value ? tableRef.value.validate() : true
}

// 行数据添加时初始化
function rowInit(row) {
  return tableRef.value.rowInit(row)
}

function init() {
  if (matSpecRef.value) {
    matSpecRef.value.clear()
  }
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
  height: 100%;
}
</style>
