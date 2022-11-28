<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="75%"
    title="研发费用占比配置"
    :wrapper-closable="false"
    :before-close="handleClose"
  >
    <template #titleRight>
      <span v-permission="permission.set">
        <common-button v-show="isEditMode" size="mini" type="primary" @click="preview">保存预览</common-button>
        <common-button v-show="isEditMode" size="mini" type="danger" @click="handleCancelEdit">退出编辑</common-button>
        <common-button v-show="!isEditMode" size="mini" type="primary" @click="isEditMode = true">编辑</common-button>
      </span>
    </template>
    <template #content>
      <div class="class-unit-config">
        <div class="query-content">
          <el-input
            v-model.trim="query.nameOrCode"
            placeholder="输入科目名称、编号搜索"
            class="filter-item"
            style="width: 200px; margin-bottom: 15px"
            size="small"
            clearable
          />
        </div>
        <common-table
          ref="table"
          v-loading="loading"
          border
          :data="filterList"
          :cell-class-name="changedCellMask"
          :max-height="maxHeight"
          :highlight-current-row="false"
          :span-method="spanMethod"
          row-key="id"
        >
          <el-table-column type="index" label="序号" align="center" width="60" />
          <el-table-column key="first" prop="first" label="一级" align="left" min-width="160px">
            <template #header>
              <el-tooltip effect="light" :content="`一级科目（名称-编号）`" placement="top">
                <div>
                  <span>一级科目</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template #default="{ row }">
              <span class="classify-name">{{ `${row.fullName[0]}-${row.fullCode[0]}` }}</span>
            </template>
          </el-table-column>
          <el-table-column v-if="level > 1" key="second" prop="second" label="二级" align="left" min-width="160px">
            <template v-slot:header>
              <el-tooltip effect="light" :content="`二级科目（名称-编号）`" placement="top">
                <div>
                  <span>二级科目</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template #default="{ row }">
              <span class="classify-name">{{ `${row.fullName[1]}-${row.fullCode[1]}` }}</span>
            </template>
          </el-table-column>
          <el-table-column v-if="level > 2" key="third" prop="third" label="三级" align="left" min-width="160px">
            <template v-slot:header>
              <el-tooltip effect="light" :content="`三级科目（名称-编号）`" placement="top">
                <div style="margin: 0 10px">
                  <span>三级科目</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template #default="{ row }">
              <span class="classify-name">{{ `${row.fullName[2]}-${row.fullCode[2]}` }}</span>
            </template>
          </el-table-column>
          <el-table-column
            key="sourceRdRate"
            prop="sourceRdRate"
            :show-overflow-tooltip="true"
            label="研发费用占比（%）"
            min-width="100px"
            align="center"
          >
            <template #default="{ row }">
              <template v-if="isEditMode">
                <common-input-number
                  v-if="row.sourceRow"
                  v-model="row.sourceRow.rdRate"
                  :step="1"
                  :min="0"
                  :max="100"
                  :precision="2"
                  size="mini"
                  style="width: 100%"
                />
              </template>
              <div v-else>{{ row.rdRate }}</div>
            </template>
          </el-table-column>
        </common-table>
        <config-preview v-bind="$attrs" v-model:visible="previewVisible" :data="list" :level="level" @save-success="handleSaveSuccess" />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, inject, reactive, ref, watch, computed } from 'vue'
// import { measureTypeEnum } from '@enum-ms/wms'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useTableChange from '@compos/form/use-table-change'
import configPreview from './config-preview'

const emit = defineEmits(['update:visible'])

const props = defineProps({
  classificationList: {
    type: Array,
    default: () => []
  },
  level: {
    type: Number,
    default: 2
  },
  visible: {
    type: Boolean,
    default: false
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const sourceMap = inject('sourceMap')
const permission = inject('permission')

const drawerRef = ref()
const list = ref([])
const query = reactive({
  nameOrCode: ''
})

const previewVisible = ref(false)
const isEditMode = ref(false)
const loading = ref(false)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook: handleCancelEdit })

const { changedCellMask } = useTableChange({ fieldMap: sourceMap })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.common-drawer',
    extraBox: ['.el-drawer__header', '.query-content'],
    wrapperBox: ['.el-drawer__body', '.class-unit-config'],
    navbar: false,
    clientHRepMainH: true,
    extraHeight: 50,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

// 根据搜索条件过滤
const filterList = computed(() => {
  const nameOrCode = query.nameOrCode
  const _list = list.value.filter(
    (v) => v.fullName.some((v) => v.indexOf(nameOrCode) > -1) || v.fullSerialNumber.some((v) => v.indexOf(nameOrCode) > -1)
  )
  return mergeCells(_list)
})

watch(
  [() => props.visible, () => props.classificationList, () => props.level],
  ([visible]) => {
    if (visible) {
      init()
    }
  },
  { immediate: true }
)

function init() {
  loading.value = true
  // 深拷贝数组，退出编辑时还原数据
  list.value = props.classificationList
  loading.value = false
}

// 退出编辑
function handleCancelEdit() {
  isEditMode.value = false
  init()
}

// 预览窗口
function preview() {
  previewVisible.value = true
}

function mergeCells(list) {
  if (list.length === 0) return list
  const row = [[], []]
  const id = [-1, -1]
  list.forEach((v) => {
    for (let i = 0; i < row.length; i++) {
      const newId = v.fullId[i]
      const oldId = id.length > i ? id[i] : undefined
      if (newId === oldId) {
        row[i][row[i].length - 1]++
      } else {
        row[i].push(1)
        id[i] = newId
      }
    }
  })
  row[0].reduce((total, cur) => {
    list[total].name1_rowSpan = cur
    total += cur
    return total
  }, 0)
  row[1].reduce((total, cur) => {
    list[total].name2_rowSpan = cur
    total += cur
    return total
  }, 0)
  return list
}

// 合并单元格
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (props.level > 1) {
    if (column.property === 'first') {
      return {
        rowspan: row.name1_rowSpan || 0,
        colspan: 1
      }
    }
    if (props.level > 2 && column.property === 'second') {
      return {
        rowspan: row.name2_rowSpan || 0,
        colspan: 1
      }
    }
  }
}

function handleSaveSuccess() {
  isEditMode.value = false
}
</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}

.classify-name {
  padding: 0 10px;
}
::v-deep(.el-table) {
  th,
  td {
    padding: 0;
  }
  .el-tooltip {
    line-height: 40px;
  }
  .cell {
    line-height: 32px;
    padding: 0;
  }
  th .cell {
    padding: 0 10px;
  }
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }

  .cell {
    .el-input__inner {
      border: none;
    }
    .common-input-number__increase {
      border-left: none;
      margin-right: 10px;
    }
    .common-input-number__decrease {
      border-right: none;
      margin-left: 10px;
    }
  }
}
</style>
