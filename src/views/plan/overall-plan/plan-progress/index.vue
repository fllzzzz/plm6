<template>
  <div class="app-container">
    <template v-if="currentProject && currentProject.projectContentList && currentProject.projectContentList.length>0">
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
      default-expand-all
      style="width: 100%"
    >
      <el-table-column type="expand">
        <template #default="props">
          <common-table
            ref="singleTable"
            :data="[{centent:1}]"
            class="customer-table"
            style="width: 100%;border-color:transparent;"
          >
            <el-table-column key="sort" prop="sort" label="区域名称" align="center">
              <template v-slot="scope">
                <span>第一批（1~12轴线交A-F轴线）</span>
              </template>
            </el-table-column>
            <el-table-column key="sort" prop="sort" label="计划完成时间" align="center">
              <template v-slot="scope">
                <span>2021年1月1日</span>
              </template>
            </el-table-column>
            <el-table-column key="sort1" prop="sort" label="深化进度" align="center" width="360">
              <template v-slot="scope">
                <div style="position:relative;">
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);left:0;">2021/01/01丨已进行/2天</span>
                  <el-progress :stroke-width="18" :percentage="20" :color="'#489fef'" />
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);right:50px;">2021/01/12丨总工期/12天</span>
                </div>
                <div style="position:relative;margin-top:5px;">
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);left:0;">已完成/50(t)</span>
                  <el-progress :stroke-width="22" :percentage="20" :color="'#13ce66'" />
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);right:50px;">总工作量/100(t)</span>
                </div>
              </template>
            </el-table-column>
            <el-table-column key="sort2" prop="sort" label="加工进度" align="center" width="360">
              <template v-slot="scope">
                <div style="position:relative;">
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);left:0;">2021/01/01丨已进行/2天</span>
                  <el-progress :stroke-width="18" :percentage="20" :color="'#489fef'" />
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);right:50px;">2021/01/12丨总工期/12天</span>
                </div>
                <div style="position:relative;margin-top:5px;">
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);left:0;">已完成/50(t)</span>
                  <el-progress :stroke-width="22" :percentage="20" :color="'#13ce66'" />
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);right:50px;">总工作量/100(t)</span>
                </div>
              </template>
            </el-table-column>
            <el-table-column key="sort3" prop="sort" label="安装进度" align="center" width="360">
              <template v-slot="scope">
                <div style="position:relative;">
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);left:0;">2021/01/01丨已进行/2天</span>
                  <el-progress :stroke-width="18" :percentage="20" :color="'#489fef'" />
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);right:50px;">2021/01/12丨总工期/12天</span>
                </div>
                <div style="position:relative;margin-top:5px;">
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);left:0;">已完成/50(t)</span>
                  <el-progress :stroke-width="22" :percentage="20" :color="'#13ce66'" />
                  <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);right:50px;">总工作量/100(t)</span>
                </div>
              </template>
            </el-table-column>
          </common-table>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="单体" min-width="100" />
      <el-table-column v-if="columns.visible('axis')" key="axis" prop="axis" :show-overflow-tooltip="true" label="计划完成时间" min-width="180" />
      <el-table-column v-if="columns.visible('axis')" key="axis" prop="axis" :show-overflow-tooltip="true" label="工作量(t)" min-width="180" >
        <template v-slot="scope">
          <div style="position:relative;">
            <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);left:0;">已完成/50(t)</span>
            <el-progress :stroke-width="22" :percentage="20" :color="'#13ce66'" />
            <span style="font-size:12px;position:absolute;z-index:200;top:50%;transform:translateY(-50%);right:50px;">总工作量/100(t)</span>
          </div>
        </template>
      </el-table-column>
    </common-table>
      <!--分页组件-->
      <pagination />
      <!-- <mForm :project-id="globalProjectId" :type-info="typeInfo" /> -->
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
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'

const { currentProject, globalProjectId } = mapGetters(['currentProject','globalProjectId'])
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
const typeInfo = ref([])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '区域计划',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['productType'],
    crudApi: { ...crudApi },
    hasPagination: true
  }
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
    row.dateDifference = dateDifference(row.startDate, row.endDate) + '天'
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
      v.dateDifference = dateDifference(v.startDate, v.endDate) + '天'
    } else {
      v.dateDifference = ''
    }
    v.sourceRemark = v.remark
    v.sourceStartDate = v.startDate
    v.sourceEndDate = v.endDate
    v.startDate = v.startDate ? v.startDate + '' : undefined
    v.endDate = v.endDate ? v.endDate + '': undefined
    v.modifying = false
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.projectId = globalProjectId
  return !!crud.form.projectId
}
</script>
<style lang="scss" scoped>
.customer-table{
  ::v-deep(th){
    border:none;
  }
  ::v-deep(td){
    border:none;
  }
  ::v-deep(th.is-leaf){
    border:none;
  }
  &::before{
    width: 0;
  }
}
</style>
