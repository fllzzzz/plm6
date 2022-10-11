<template>
  <div class="app-container">
      <div v-if="globalProject?.businessType === businessTypeEnum.INSTALLATION.V">
        <mHeader/>
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
        style="width: 100%;margin-top:10px;"
        :span-method="objectSpanMethod"
      >
        <el-table-column v-if="columns.visible('monomerName')" key="monomerName" prop="monomerName" align="center" :show-overflow-tooltip="true" label="单体" />
        <el-table-column
          v-if="columns.visible('content')"
          key="content"
          prop="content"
          label="项目内容"
          align="center"
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
              <div v-else class="sandwich-cell-bottom">-</div>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('startDate')"
            key="startDate"
            prop="startDate"
            label="总计划"
            align="center"
          >
            <template v-slot="scope">
              <template v-if="scope.row.startDate && scope.row.endDate">
                <div>{{`${parseTime(scope.row.startDate,'{y}-{m}-{d}')}~${parseTime(scope.row.endDate,'{y}-{m}-{d}')}`}}</div>
                <div><span style="margin-right:2px;">{{`${dateDifference(scope.row.startDate,scope.row.endDate)}`}}</span>天</div>
              </template>
              <template v-else>-</template>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('quantity')"
            key="quantity"
            prop="quantity"
            label="清单数"
            align="center"
          >
            <template v-slot="scope">
              <div>{{scope.row.quantity}}</div>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('deepenMete')"
            key="deepenMete"
            prop="deepenMete"
            label="清单量"
            align="center"
          >
            <template v-slot="scope">
              <div>{{`${scope.row.deepenMete.toFixed(scope.row.decimal)}`}}<span style="margin-left:2px;">{{`${scope.row.unit}`}}</span></div>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('areaList')"
            key="areaList"
            prop="areaList"
            label="分包单位"
            align="center"
          >
            <template v-slot="scope">
              <template v-if="scope.row.areaList.length > 0">
                <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                  <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                    <common-select
                      v-if="k.isModify"
                      v-model="k.supplierId"
                      :options="scope.row.supplierList"
                      type="other"
                      size="small"
                      :dataStructure="typeProp"
                      clearable
                      placeholder="分包单位"
                      @change="supplierChange(scope.row,i)"
                    />
                    <span v-else>{{ k.supplierName || '-' }}</span>
                  </div>
                </div>
              </template>
              <div v-else class="sandwich-cell-bottom">-</div>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('install')"
            key="install"
            prop="install"
            label="完工时间"
            align="center"
            min-width="150"
          >
            <template v-slot="scope">
              <template v-if="scope.row.areaList.length > 0">
                <div v-for="(k,i) in scope.row.areaList" :key="k.id">
                  <div :class="i===scope.row.areaList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                    <el-date-picker
                      v-if="k.isModify"
                      v-model="k.timeArr"
                      type="daterange"
                      range-separator=":"
                      size="small"
                      class="date-item filter-item"
                      value-format="x"
                      start-placeholder="开始"
                      end-placeholder="结束"
                      :disabledDate="(date) => {return date.getTime() < globalProject?.startDate}"
                      @change="timeChange(k)"
                    />
                    <span v-else>{{k?.startDate && k?.endDate? parseTime(k?.startDate,'{y}-{m}-{d}')+' : '+parseTime(k?.endDate,'{y}-{m}-{d}'): '-'}}</span>
                  </div>
                </div>
              </template>
              <div v-else class="sandwich-cell-bottom">-</div>
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
                      <common-button v-loading="submitLoading" type="info" size="mini" @click="rowCancel(scope.row,i)">取消</common-button>
                      <common-button v-loading="submitLoading" type="primary" size="mini" @click="rowSubmit(scope.row,i)">保存</common-button>
                    </template>
                    <template v-else>
                      <common-button size="mini" @click="handleRow(scope.row,i)" icon="el-icon-edit" type="primary" v-permission="permission.edit" :disabled="isModify"/>
                    </template>
                  </div>
                </div>
              </template>
              <div v-else class="sandwich-cell-bottom"></div>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div v-else>
        <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
      </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/subcontract-plan'
import { ref, watch } from 'vue'

import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'
import { DP } from '@/settings/config'
import { parseTime } from '@/utils/date'
import { subcontractPlanPM as permission } from '@/page-permission/project'
import { TechnologyTypeAllEnum, businessTypeEnum } from '@enum-ms/contract'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { ElMessage } from 'element-plus'
import { mapGetters } from '@/store/lib'

import mHeader from './module/header'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const typeProp = { key: 'supplierId', label: 'supplierName', value: 'supplierId' }
const tableRef = ref()
const originDetailRow = ref({})
const isModify = ref(false)
const submitLoading = ref(false)
const { crud, columns, CRUD } = useCRUD(
  {
    title: '分包工期制定',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.subcontract-plan',
  paginate: true,
  extraHeight: 100
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.query.monomerId = undefined
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

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

function supplierChange(row, index) {
  const val = row.areaList[index].supplierId ? row.supplierList.find(v => v.supplierId === row.areaList[index].supplierId) : {}
  row.areaList[index].supplierName = row.areaList[index].supplierId ? val.supplierName : undefined
}

function timeChange(value) {
  value.startDate = value.timeArr[0]
  value.endDate = value.timeArr[1]
}

function handleRow(row, index) {
  originDetailRow.value = JSON.parse(JSON.stringify(row.areaList[index]))
  row.areaList[index].isModify = true
  isModify.value = true
}

function rowCancel(row, index) {
  row.areaList[index] = Object.assign(row.areaList[index], JSON.parse(JSON.stringify(originDetailRow.value)))
  row.areaList[index].isModify = false
  isModify.value = false
}

async function rowSubmit(row, index) {
  if (!isNotBlank(row.areaList[index].timeArr) || !row.areaList[index].supplierId) {
    ElMessage.error('分包单位和安装计划必填')
    return
  }
  submitLoading.value = true
  try {
    const submitData = {
      endDate: row.areaList[index].timeArr[1],
      id: row.areaList[index].id,
      startDate: row.areaList[index].timeArr[0],
      supplierId: row.areaList[index].supplierId
    }
    await crudApi.edit(submitData)
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row.areaList[index].isModify = false
    crud.toQuery()
    isModify.value = false
  } catch (e) {
    console.log(`修改`, e)
  } finally {
    submitLoading.value = false
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  const showData = []
  data.data.content.map(v => {
    if (v.monomerDetailList.length > 0) {
      v.monomerDetailList.map((k, index) => {
        k.supplierList = v.supplierList
        k.monomerName = v.name
        k.decimal = k.type === TechnologyTypeAllEnum.STRUCTURE.V ? DP.COM_WT__KG : DP.MES_ENCLOSURE_L__M
        k.unit = k.type === TechnologyTypeAllEnum.STRUCTURE.V ? 't' : 'm'
        if (index === 0) {
          k.monomerNameSpan = v.monomerDetailList.length
        }
        if (k.areaList && k.areaList.length > 0) {
          k.areaList.map((value, index) => {
            value.imeArr = []
            if (value.startDate && value.endDate) {
              value.timeArr = [value.startDate, value.endDate]
            }
          })
        }
        showData.push(k)
      })
    }
  })
  data.data.content = showData || []
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
