<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <!--表格渲染-->
      <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      :stripe="false"
      class="upload-table"
      style="width: 100%"
      :span-method="objectSpanMethod"
    >
      <el-table-column v-if="columns.visible('monomerName')" key="monomerName" prop="monomerName" align="center" :show-overflow-tooltip="true" label="单体" />
      <el-table-column
        v-if="columns.visible('content')"
        key="content"
        prop="content"
        label="项目内容"
        align="center"
        width="70"
      >
        <template v-slot="scope">
          {{scope.row.type?TechnologyTypeAllEnum.VL[scope.row.type]:'-'}}
        </template>
      </el-table-column>
       <el-table-column
          v-if="columns.visible('unit')"
          key="unit"
          prop="unit"
          label="单元"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{k.name}}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('type')"
          key="type"
          prop="type"
          label="生产方式"
          width="70"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{isNotBlank(k.type)?manufactureTypeEnum.VL[k.type]:'-'}}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('deepen')"
          key="deepen"
          prop="deepen"
          label="深化计划"
          align="center"
          min-width="150"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <template v-if="isNotBlank(k.deepVal)">
                    <el-date-picker
                      v-if="k.isModify"
                      v-model="k.deepVal.timeArr"
                      type="daterange"
                      range-separator=":"
                      size="small"
                      class="date-item filter-item"
                      value-format="x"
                      start-placeholder="开始"
                      end-placeholder="结束"
                      @change="timeChange(k.deepVal,k)"
                      :disabledDate="(date) => {return (scope.row.startDate?date.getTime() < scope.row.startDate:date.getTime() < globalProject.startDate) || date.getTime() > k.date}"
                    />
                    <span>{{k.deepVal?.startDate && k.deepVal?.endDate? parseTime(k.deepVal.startDate,'{y}-{m}-{d}')+' : '+parseTime(k.deepVal.endDate,'{y}-{m}-{d}'): '-'}}</span>
                  </template>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('process')"
          key="process"
          prop="process"
          label="加工计划"
          align="center"
          min-width="150"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <template v-if="isNotBlank(k.processVal)">
                    <el-date-picker
                      v-if="k.isModify"
                      v-model="k.processVal.timeArr"
                      type="daterange"
                      range-separator=":"
                      size="small"
                      class="date-item filter-item"
                      value-format="x"
                      start-placeholder="开始"
                      end-placeholder="结束"
                      @change="timeChange(k.processVal,k)"
                      :disabled="!isNotBlank(k.deepVal.timeArr)"
                      :disabledDate="(date) => {if (k.deepVal.startDate) { return date.getTime() < k.deepVal.startDate || date.getTime() > k.date } else { return (scope.row.startDate?date.getTime() < scope.row.startDate:date.getTime() < globalProject.startDate) || date.getTime() > k.date }}"
                    />
                    <span>{{k.processVal?.startDate && k.processVal?.endDate? parseTime(k.processVal.startDate,'{y}-{m}-{d}')+' : '+parseTime(k.processVal.endDate,'{y}-{m}-{d}'): '-'}}</span>
                  </template>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('delivery')"
          key="delivery"
          prop="delivery"
          label="发运计划"
          align="center"
          min-width="150"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <template v-if="isNotBlank(k.deliveryVal)">
                    <el-date-picker
                      v-if="k.isModify"
                      v-model="k.deliveryVal.timeArr"
                      type="daterange"
                      range-separator=":"
                      size="small"
                      class="date-item filter-item"
                      value-format="x"
                      start-placeholder="开始"
                      end-placeholder="结束"
                      @change="timeChange(k.deliveryVal,k)"
                      :disabled="!isNotBlank(k.processVal.timeArr)"
                      :disabledDate="(date) => {if (k.processVal.startDate) { return date.getTime() < k.processVal.startDate || date.getTime() > k.date } else { return (scope.row.startDate?date.getTime() < scope.row.startDate:date.getTime() < globalProject.startDate) || date.getTime() > k.date }}"
                    />
                    <span>{{k.deliveryVal?.startDate && k.deliveryVal?.endDate? parseTime(k.deliveryVal.startDate,'{y}-{m}-{d}')+' : '+parseTime(k.deliveryVal.endDate,'{y}-{m}-{d}'): '-'}}</span>
                  </template>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('install') && globalProject.businessType === businessTypeEnum.INSTALLATION.V"
          key="install"
          prop="install"
          label="安装计划"
          align="center"
          min-width="150"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <template v-if="isNotBlank(k.installVal)">
                    <el-date-picker
                      v-if="k.isModify"
                      v-model="k.installVal.timeArr"
                      type="daterange"
                      range-separator=":"
                      size="small"
                      class="date-item filter-item"
                      value-format="x"
                      start-placeholder="开始"
                      end-placeholder="结束"
                      @change="timeChange(k.installVal,k)"
                      :disabled="!(isNotBlank(k.processVal.timeArr)&&isNotBlank(k.deepVal.timeArr))"
                      :disabledDate="(date) => {if(k.deliveryVal.startDate){ return date.getTime() < k.deliveryVal.startDate || date.getTime() > k.date} else { return (scope.row.startDate?date.getTime() < scope.row.startDate:date.getTime() < globalProject.startDate) || date.getTime() > k.date }}"
                    />
                    <span>{{k.installVal?.startDate && k.installVal?.endDate? parseTime(k.installVal.startDate,'{y}-{m}-{d}')+' : '+parseTime(k.installVal.endDate,'{y}-{m}-{d}'): '-'}}</span>
                  </template>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalDays')"
          key="totalDays"
          prop="totalDays"
          label="用时(天)"
          align="center"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  {{k.totalDays?k.totalDays:'-'}}
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
        <el-table-column
          label="操作"
          width="160px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <template v-if="scope.row.areaList.length > 0">
              <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                  <template v-if="k.isModify">
                    <common-button type="info" size="mini" @click="rowCancel(scope.row,i)">取消</common-button>
                    <common-button type="primary" size="mini" @click="rowSubmit(scope.row,i)">保存</common-button>
                  </template>
                  <template v-else>
                    <common-button size="mini" @click="handleRow(scope.row,i)" icon="el-icon-edit" type="primary" v-permission="permission.edit"/>
                  </template>
                </div>
              </div>
            </template>
            <div v-else class="sandwich-cell-bottom"></div>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
    </template>
    <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/plan-make'
import { ref, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import { businessTypeEnum } from '@enum-ms/contract'
import { manufactureTypeEnum, areaPlanTypeEnum } from '@enum-ms/plan'
import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'
import { parseTime } from '@/utils/date'
import { planMakeListPM as permission } from '@/page-permission/plan'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { ElMessage } from 'element-plus'

import pagination from '@crud/Pagination'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const originDetailRow = ref({})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '区域计划',
    sort: ['id.desc'],
    permission: { ...permission.value },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-make',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function objectSpanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 0) {
    if (row.monomerNameSpan) {
      return {
        rowspan: row.monomerNameSpan,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 1
      }
    }
  }
}

function handleRow(row, index) {
  originDetailRow.value = JSON.parse(JSON.stringify(row.areaList[index]))
  row.areaList[index].isModify = true
}

function rowCancel(row, index) {
  row.areaList[index] = Object.assign(row.areaList[index], JSON.parse(JSON.stringify(originDetailRow.value)))
  row.areaList[index].isModify = false
}

function timeChange(value, k) {
  if (isNotBlank(value.timeArr)) {
    value.startDate = value.timeArr[0]
    value.endDate = value.timeArr[1]
  } else {
    value.startDate = undefined
    value.endDate = undefined
  }
  totalTime(k)
}

function totalTime(k) {
  const startDate = k.deepVal?.startDate || k.processVal?.startDate || k.deliveryVal?.startDate || k.installVal?.startDate || undefined
  const endDate = k.installVal?.endDate || k.deliveryVal?.endDate || k.processVal?.endDate || k.deepVal?.endDate || undefined
  if (startDate && endDate) {
    k.totalDays = dateDifference(startDate, endDate)
  }
}
async function rowSubmit(row, index) {
  if (!isNotBlank(row.areaList[index].deepVal.timeArr)) {
    ElMessage.error('深化计划必填')
    return
  }
  if (!isNotBlank(row.areaList[index].processVal.timeArr)) {
    ElMessage.error('生产计划必填')
    return
  }
  if (!isNotBlank(row.areaList[index].deliveryVal.timeArr)) {
    ElMessage.error('发运计划必填')
    return
  }
  if (globalProject.value.businessType === businessTypeEnum.INSTALLATION.V && isNotBlank(originDetailRow.value.installVal.timeArr) && !isNotBlank(row.areaList[index].installVal.timeArr)) {
    ElMessage.error('安装计划必填')
    return
  }
  try {
    const data = [{ ...row.areaList[index].deepVal }, { ...row.areaList[index].processVal }, { ...row.areaList[index].deliveryVal }]
    if (globalProject.value.businessType === businessTypeEnum.INSTALLATION.V) {
      data.push({ ...row.areaList[index].installVal })
    }
    await crudApi.edit(row.areaList[index].id, data)
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row.areaList[index].isModify = false
  } catch (e) {
    console.log(`修改`, e)
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  const showData = []
  data.data.content.map(v => {
    if (v.monomerDetailList.length > 0) {
      v.monomerDetailList.map((k, index) => {
        k.startDate = v.startDate
        k.monomerName = v.name
        if (index === 0) {
          k.monomerNameSpan = v.monomerDetailList.length
        }
        if (k.areaList && k.areaList.length > 0) {
          k.areaList.map((value, index) => {
            const deepVal = value.planDetailList.find(m => m.type === areaPlanTypeEnum.DEEPEN.V)
            const processVal = value.planDetailList.find(m => m.type === areaPlanTypeEnum.PROCESS.V)
            const installVal = value.planDetailList.find(m => m.type === areaPlanTypeEnum.INSTALL.V)
            const deliveryVal = value.planDetailList.find(m => m.type === areaPlanTypeEnum.DELIVERY.V) || {}
            if (deepVal) {
              deepVal.timeArr = []
              if (deepVal.startDate && deepVal.endDate) {
                deepVal.timeArr = [deepVal.startDate, deepVal.endDate]
              }
            }
            if (processVal) {
              processVal.timeArr = []
              if (processVal.startDate && processVal.endDate) {
                processVal.timeArr = [processVal.startDate, processVal.endDate]
              }
            }
            if (installVal && installVal.startDate && installVal.endDate) {
              installVal.timeArr = []
              if (installVal.startDate && installVal.endDate) {
                installVal.timeArr = [installVal.startDate, installVal.endDate]
              }
            }
            if (deliveryVal) {
              deliveryVal.timeArr = []
              deliveryVal.type = areaPlanTypeEnum.DELIVERY.V
              if (deliveryVal.startDate && deliveryVal.endDate) {
                deliveryVal.timeArr = [deliveryVal.startDate, deliveryVal.endDate]
              }
            }
            value.deepVal = deepVal
            value.processVal = processVal
            value.installVal = installVal
            value.deliveryVal = deliveryVal
            totalTime(value)
          })
        }
        showData.push(k)
      })
    }
  })
  console.log(showData)
  data.data.content = showData || []
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.projectId = globalProjectId.value
  return !!crud.form.projectId
}
</script>
<style lang="scss" scoped>
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
}
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
  // ::v-deep(.el-input__inner) {
  //   padding: 0;
  //   padding-left: 2px;
  // }
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
::v-deep(.el-table--small .el-table__cell){
  padding:4px 0;
}
</style>
