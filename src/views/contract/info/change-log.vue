<template>
  <div>
    <div style="margin-bottom:20px;">
      <common-select
        :value.sync="query.projectType"
        :options="projectTypeEnum"
        type="enum"
        size="small"
        clearable
        placeholder="变更类型"
        style="width:150px"
      />
      <common-select
        :value.sync="query.projectType"
        :options="projectTypeEnum"
        type="enum"
        size="small"
        clearable
        placeholder="来源"
        style="width:150px"
      />
      <common-select
        :value.sync="query.projectType"
        :options="projectTypeEnum"
        type="enum"
        size="small"
        clearable
        placeholder="状态"
        style="width:150px"
      />
      <el-input
        v-model="query.singerId"
        size="small"
        placeholder="请输入负责人"
        style="width: 150px;"
        clearable
        @keyup.enter.native="fetchLogListInput"
      />
      <span>
        <el-button size="mini" type="success" icon="el-icon-search">搜索</el-button>
        <el-button size="mini" type="warning" icon="el-icon-refresh-left">重置</el-button>
      </span>
    </div>
    <!-- 表格渲染 -->
    <el-table
      ref="table"
      v-loading="tableLoading"
      border
      :stripe="$TBS.STRIPE"
      :data="tableData"
      :max-height="$_height"
      style="width: 100%;"
    >
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
      <el-table-column prop="project.shortName" label="变更类型" min-width="180">
        <template v-slot="scope">
          <el-tag v-if="scope.row.type" :type="operationTypeEnumV[scope.row.type].T">{{ operationTypeEnumV[scope.row.type].L | emptyTextFormatter }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="operator" label="金额" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.operator | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="operator" label="合同金额" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.operator | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="operator" label="负责人" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.operator | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="createTime" label="变更日期" align="center" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.createTime | parseTime('{y}-{m}-{d} {h}:{i}') | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="createTime" label="创建日期" align="center" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.createTime | parseTime('{y}-{m}-{d} {h}:{i}') | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="operator" label="创建人" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.operator | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="createTime" label="审核日期" align="center" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.createTime | parseTime('{y}-{m}-{d} {h}:{i}') | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="operator" label="来源" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.operator | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="operator" label="状态" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.operator | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="description" label="操作">
        <template v-slot="scope">
          <el-button type="primary" size="mini" @click.stop="openDetail(scope.row)">查看详情</el-button>
          <!--          <span>{{ scope.row.field?(scope.row.field | emptyTextFormatter):'' }}</span>-->
          <!--          <span v-html="scope.row.description" />-->
        </template>
      </el-table-column>
    </el-table>
    <!--分页组件-->
    <el-pagination
      :total="total"
      :current-page="query.page"
      :page-size="query.size"
      style="margin-top: 8px;"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
  </div>
</template>

<script>
import { changeLog as get } from '@/api/contract/project'
import enumOperate, { contractOperationTypeEnum as operationTypeEnum, projectTypeEnum } from '@/utils/enum/index'
import sizeCalc from '@/mixins/sizeCalc'
import { debounce } from '@/utils'
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/utils/constant'

const monthDefault = moment().format('YYYY-MM')

const operationTypeEnumV = enumOperate.getVal(operationTypeEnum)

export default {
  components: {},
  // inject: ['permission'],
  mixins: [sizeCalc],
  // props: {
  //   visible: {
  //     type: Boolean,
  //     required: true
  //   }
  // },
  data() {
    return {
      extraHeight: 180,
      operationTypeEnum,
      operationTypeEnumV,
      projectTypeEnum,
      timeArr: [moment(monthDefault).startOf('month').valueOf(), moment().valueOf()],
      pickerOptions: {
        shortcuts: PICKER_OPTIONS_SHORTCUTS
      },
      query: {
        page: 1,
        size: this.$TBS.PAGE_SIZE,
        projectId: undefined,
        operator: undefined,
        type: undefined,
        startTime: moment(monthDefault).startOf('month').valueOf(),
        endTime: moment().valueOf()
      },
      total: 0,
      tableLoading: false,
      detailVisible: false,
      tableData: [],
      detailId: null
    }
  },
  computed: {
    drawerVisible() {
      return this.visible
    }
  },
  watch: {
    visible(val) {
      if (val) {
        this.fetchLogList()
      }
    }
  },
  methods: {
    fetchLogList: debounce(async function() {
      try {
        this.tableLoading = true
        const { content, totalElements } = await get(this.query)
        this.tableData = content
        this.total = totalElements
      } catch (error) {
        console.log('项目变更记录', error)
      } finally {
        this.tableLoading = false
      }
    }, 100, false),
    fetchLogListInput: debounce(async function() {
      try {
        this.tableLoading = true
        const { content, totalElements } = await get(this.query)
        this.tableData = content
        this.total = totalElements
      } catch (error) {
        console.log('项目变更记录', error)
      } finally {
        this.tableLoading = false
      }
    }, 1000, false),
    handleSizeChange(val) {
      this.query.page = 1
      this.query.size = val
      this.fetchLogList()
    },
    handleCurrentChange(val) {
      this.query.page = val
      this.fetchLogList()
    },
    handleDateChange() {
      if (this.timeArr && this.timeArr.length > 1) {
        this.query.startTime = this.timeArr[0]
        this.query.endTime = this.timeArr[1]
      } else {
        this.query.startTime = undefined
        this.query.endTime = undefined
      }
      this.fetchLogList()
    },
    openDetail(row) {
      this.detailId = row.id
      this.detailVisible = true
    },
    handleClose() {
      this.$emit('update:visible', false)
    }
  }

}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  /deep/ .el-input-number .el-input__inner {
    text-align: left;
  }
</style>
