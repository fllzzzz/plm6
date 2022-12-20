<template>
   <common-drawer
    ref="drawerRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="计划排期"
    :wrapper-closable="false"
    size="80%"
    custom-class="schedule-detail"
  >
    <template #titleRight>
      <common-button v-loading="submitLoading" size="mini" type="warning" @click="isEdit=true" v-if="!isEdit && checkPermission(permission.edit)">创建计划</common-button>
      <common-button v-loading="submitLoading" size="mini" type="primary" @click="onSubmit(auditTypeEnum.PASS.V)" v-if="isEdit">提交</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="130px">
        <common-table :data="list" v-loading="tableLoading" return-source-data :showEmptySymbol="false" :span-method="objectSpanMethod" :max-height="maxHeight" :cell-class-name="wrongCellMask" :stripe="false">
          <el-table-column key="project" prop="project" label="项目" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
            </template>
          </el-table-column>
          <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" :show-overflow-tooltip="true" />
          <el-table-column key="area.name" prop="area.name" label="区域" align="center" :show-overflow-tooltip="true" />
          <el-table-column key="quantity" prop="quantity" label="数量（件）" align="center" :show-overflow-tooltip="true" />
          <el-table-column key="totalNetWeight" prop="totalNetWeight" label="重量（kg）" align="center" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <span v-if="scope.row.totalNetWeight">{{toThousand(scope.row.totalNetWeight,DP.COM_WT__KG)}}</span>
              <span v-else>-</span>
            </template>
          </el-table-column>
          <el-table-column key="totalWeight" prop="totalWeight" label="合计（kg）" align="center" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <span v-if="scope.row.totalWeight" style="cursor:pointer;color:#409eff;" @click="drawerVisible=true">{{toThousand(scope.row.totalWeight,DP.COM_WT__KG)}}</span>
              <span v-else>-</span>
            </template>
          </el-table-column>
          <el-table-column key="closingDate" prop="closingDate" label="截止日" align="center" :show-overflow-tooltip="true">
            <template v-slot="scope">
              {{scope.row.closingDate?parseTime(scope.row.closingDate,'{y}-{m}-{d}'):'-'}}
            </template>
          </el-table-column>
          <el-table-column key="constructionDay" prop="constructionDay" label="工期（天）" align="center" />
          <el-table-column key="line" prop="line" label="选择车间" align="center" width="290">
            <template v-slot="scope">
              <el-cascader
                v-if="isEdit"
                v-model="scope.row.line"
                :options="options"
                :props="cascaderProps"
                filterable
                clearable
                show-all-levels
                placeholder="请选择"
                @change="handleChange(scope.row.line,scope.row)"
                style="width:250px;"
                :disabled="!scope.row.quantity"
              />
              <span v-else>{{scope.row.workshop?.id?scope.row.workshop.name+(scope.row.productionLine?.id?'/'+scope.row.productionLine.name:''):'-'}}</span>
            </template>
          </el-table-column>
          <el-table-column key="timeArr" prop="timeArr" label="交货日期" align="center" width="250">
            <template v-slot="scope">
              <el-date-picker
                v-if="isEdit"
                v-model="scope.row.endDate"
                type="date"
                value-format="x"
                placeholder="请选择交货日期"
                :disabled="!scope.row.quantity"
                @change="timeChange(scope.row)"
              />
              <template v-else>
                <span v-if="scope.row.endDate">{{`${parseTime(scope.row.endDate,'{y}-{m}-{d}')}`}}</span>
                <span v-else>-</span>
              </template>
            </template>
          </el-table-column>
        </common-table>
      </el-form>
      <common-drawer
        append-to-body
        ref="detailRef"
        v-model="drawerVisible"
        top="10vh"
        :before-close="()=>{
          drawerVisible=false
        }"
        title="分段清单"
        :wrapper-closable="false"
        custom-class="artifact-tree-drawer"
        size="90%"
      >
        <template #content>
          <box-list :drawerVisible="drawerVisible" :currentId="currentId"/>
        </template>
      </common-drawer>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch, computed } from 'vue'
import { scheduleDetail, updateSchedule } from '@/api/bridge/production-order-manage/production-order'
import { ElNotification, ElMessage, ElMessageBox } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useBridgeProductLines from '@compos/store/use-bridge-product-lines'
import { projectNameFormatter } from '@/utils/project'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import useVisible from '@compos/use-visible'
import { auditTypeEnum } from '@enum-ms/contract'
import { isNotBlank } from '@data-type/index'
import { DP } from '@/settings/config'
import { parseTime } from '@/utils/date'
import { toThousand } from '@/utils/data-type/number'
import { judgeSameValue } from '@/views/contract/info/judgeSameValue'
import checkPermission from '@/utils/system/check-permission'
import useTableValidate from '@compos/form/use-table-validate'

import boxList from './box-list'

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const props = defineProps({
  currentId: {
    type: [String, Number],
    default: undefined
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { productLines } = useBridgeProductLines()

const submitLoading = ref(false)
const list = ref([])
const options = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const isEdit = ref(false)
const originData = ref([])
const detailRef = ref()
const drawerVisible = ref(false)

const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'name',
    children: 'children',
    expandTrigger: 'hover',
    checkStrictly: true
  }
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.schedule-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

watch(
  productLines,
  (val) => {
    setOptions(productLines)
  },
  { immediate: true, deep: true }
)

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      isEdit.value = false
      fetchDetail()
    }
  },
  { deep: true, immediate: true }
)

// 车间产线
const validateLine = (value, row) => {
  if (isNotBlank(row.timeArr)) {
    if (isNotBlank(value)) {
      return true
    } else {
      return false
    }
  }
  return true
}

const tableRules = {
  line: [{ validator: validateLine, message: '请选择车间', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

function objectSpanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 0 || columnIndex === 1 || columnIndex === 5) {
    return {
      rowspan: (columnIndex === 0 || columnIndex === 5) ? (row.projectSpan || 0) : (row.monomerSpan || 0),
      colspan: 1
    }
  }
}

// 设置级联数据
function setOptions(tree) {
  options.value = []
  const workshopTree = []
  try {
    if (tree) {
      const treeData = JSON.parse(JSON.stringify(tree.value))
      treeData.map(v => {
        v.disabled = true
        v.workshopList.map(k => {
          k.children = k.productionLineList.filter(val => val.productType === bridgeComponentTypeEnum.BOX.V) || []
          workshopTree.push(k)
        })
        v.children = v.workshopList || []
      })
      options.value = workshopTree
    }
  } catch (error) {
    console.log('获取工厂车间生产线树失败', error)
  }
}

function handleChange(val, row) {
  if (val && val.length) {
    const filterVal = options.value.find(v => v.id === val[0])
    row.factoryId = filterVal.factoryId
    row.workshopId = val[0]
    row.productionLineId = val[1] || undefined
  } else {
    row.factoryId = undefined
    row.workshopId = undefined
    row.productionLineId = undefined
  }
}

function timeChange(value) {
  if (value.endDate) {
    value.startDate = new Date().getTime()
  } else {
    value.startDate = undefined
  }
}

async function fetchDetail() {
  list.value = []
  if (!props.currentId) {
    return
  }
  try {
    const { content } = await scheduleDetail({ projectId: props.currentId })
    const monomerArr = []
    let totalWeight = 0
    content.map(v => {
      totalWeight += v.totalNetWeight
      v.line = []
      v.projectId = v.project?.id
      v.monomerId = v.monomer?.id
      v.areaId = v.area?.id
      v.factoryId = v.factory?.id
      v.workshopId = v.workshop?.id
      v.productionLineId = v.productionLine?.id
      if (v.productionLineId) {
        v.line.push(v.workshopId, v.productionLineId)
      } else {
        if (v.workshopId) {
          v.line.push(v.workshopId)
        }
      }
      if (monomerArr.indexOf(v.monomerId) < 0) {
        monomerArr.push(v.monomerId)
      }
    })
    for (let i = 0; i < monomerArr.length; i++) {
      const filterData = content.filter(k => k.monomerId === monomerArr[i])
      filterData[0].monomerSpan = filterData.length
      list.value.push(...filterData)
    }
    list.value[0].projectSpan = list.value.length
    list.value[0].totalWeight = totalWeight
    originData.value = JSON.parse(JSON.stringify(list.value))
  } catch (error) {
    console.log('生产订单详情失败', error)
  }
}

async function onSubmit() {
  const { validResult, dealList } = tableValidate(list.value)
  if (validResult) {
    list.value = dealList
  } else {
    return validResult
  }
  submitLoading.value = true
  try {
    const submitData = []
    let isTimeGreater = false
    list.value.map(v => {
      if (v.closingDate && v.endDate > v.closingDate) {
        isTimeGreater = true
      }
      const val = originData.value.find(k => k.areaId === v.areaId)
      if (!judgeSameValue(val.endDate, v.endDate) || !judgeSameValue(val.line, v.line)) {
        submitData.push({
          areaId: v.areaId,
          endDate: v.endDate || null,
          factoryId: v.factoryId || null,
          monomerId: v.monomerId,
          productionLineId: v.productionLineId || null,
          projectId: v.projectId,
          startDate: v.startDate || null,
          workshopId: v.workshopId || null
        })
      }
    })
    if (!submitData.length) {
      ElMessage.error('请至少提交或更改一条信息')
      submitLoading.value = false
      return
    }
    if (isTimeGreater) {
      await ElMessageBox.confirm('排期计划大于项目主计划，是否继续？', '提示', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      })
    }
    await updateSchedule(submitData)
    submitLoading.value = false
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    submitLoading.value = false
    console.log('生产订单排期', error)
  }
}

</script>
