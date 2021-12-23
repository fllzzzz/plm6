<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div>
      <!--表格渲染-->
      <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
       <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="区域名称" min-width="100" />
      <el-table-column v-if="columns.visible('axis')" key="axis" prop="axis" :show-overflow-tooltip="true" label="轴线/标高" min-width="180" />
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" label="制造类型" width="80">
        <template v-slot="scope">
          <el-tag effect="plain" :type="scope.row.typeTagType">{{ isNotBlank(scope.row.areaType)?manufactureTypeEnum.VL[scope.row.areaType]:'-' }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('startDate')" key="startDate" prop="startDate" label="开始日期" align="center" width="220px">
        <template v-slot="scope">
          <el-date-picker
            v-if="scope.row.modifying"
            v-model="scope.row.startDate"
            type="date"
            size="small"
            value-format="x"
            placeholder="选择日期"
            style="width:160px"
            :disabledDate="(date) => {if (scope.row.endDate) { return date.getTime() - 8.64e6 > scope.row.endDate } else { return date.getTime() - 8.64e6 > scope.row.date }}"
            @change="handleDateChange($event, scope.row)"
          />
          <template v-else>
            <span v-if="scope.row.startDate" v-parse-time="'{y}-{m}-{d}'">{{ scope.row.startDate }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('endDate')" key="endDate" prop="endDate" label="结束日期" align="center" width="220px">
        <template v-slot="scope">
          <el-date-picker
            v-if="scope.row.modifying"
            v-model="scope.row.endDate"
            type="date"
            size="small"
            value-format="x"
            placeholder="选择日期"
            style="width:160px"
            :disabledDate="(date) => {if (scope.row.startDate) { return date.getTime() < scope.row.startDate - 8.64e6 || date.getTime() - 8.64e6 > scope.row.date } else { return date.getTime() - 8.64e6 > scope.row.date }}"
            @change="handleDateChange($event, scope.row)"
          />
          <template v-else>
            <span v-if="scope.row.endDate" v-parse-time="'{y}-{m}-{d}'">{{ scope.row.endDate }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('dateDifference')" key="dateDifference" prop="dateDifference" label="计划用时" align="center" min-width="100px" />
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="160px">
        <template v-slot="scope">
          <el-input
            v-if="scope.row.modifying"
            v-model="scope.row.remark"
            style="width: 130px;"
            size="small"
          />
          <span v-else>{{ scope.row.remark }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit])"
        label="操作"
        width="160px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <template v-if="scope.row.modifying">
            <common-button type="warning" size="mini" @click.stop="handelModifying(scope.row,false)">取消</common-button>
            <common-button type="success" size="mini" @click.stop="submit(scope.row)">保存</common-button>
          </template>
          <common-button v-else type="primary" size="mini" @click.stop="handelModifying(scope.row, true)">编辑</common-button>
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
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { isNotBlank } from '@data-type/index'
import { dateDifferenceReduce } from '@/utils/date'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['plan:get'],
  edit: ['plan:edit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '区域计划',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['productType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-make',
  paginate: true,
  extraHeight: 157
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true }
)

function handelModifying(row, modifying) {
  row.modifying = modifying
  if (!modifying) {
    row.startDate = row.sourceStartDate
    row.endDate = row.sourceEndDate
    row.remark = row.sourceRemark
    this.handleDateChange('', row)
  }
}

function handleDateChange(val, row) {
  if (row.startDate && row.endDate) {
    row.dateDifference = dateDifferenceReduce(row.startDate, row.endDate) + '天'
  } else {
    row.dateDifference = ''
  }
}
async function submit(row) {
  try {
    const data = {
      id: row.id,
      startDate: row.startDate,
      endDate: row.endDate,
      remark: row.remark
    }
    await crudApi.edit(data)
    crud.notify('操作成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('区域计划保存', error)
  } finally {
    crud.toQuery()
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.typeTagType = v.type === manufactureTypeEnum.HOMEMADE.V ? '' : 'warning'
    if (v.startDate && v.endDate) {
      v.dateDifference = dateDifferenceReduce(v.startDate, v.endDate) + '天'
    } else {
      v.dateDifference = ''
    }
    v.sourceRemark = v.remark
    v.sourceStartDate = v.startDate
    v.sourceEndDate = v.endDate
    v.startDate = v.startDate ? v.startDate + '' : undefined
    v.endDate = v.endDate ? v.endDate + '' : undefined
    v.modifying = false
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.projectId = globalProjectId
  return !!crud.form.projectId
}
</script>
